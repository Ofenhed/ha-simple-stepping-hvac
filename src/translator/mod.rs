use std::{borrow::Cow, collections::HashSet, rc::Rc};

use crate::{
    automation::{
        building_blocks::Choose, Action, Automation, Condition, Service, Stop, TimeInterval,
        TraceOptions, Trigger, TriggerHolder,
    },
    config::{ClimateConfig, Room},
    entity_id::{EntityId, EntityMember, EntityMemberType, EntityType, HasEntityType as _},
    helpers::{
        DerivativeSensor,
        DeviceClass::{self, Temperature},
        Duration, InputNumber, InputNumberMode,
        StateClass::Measurement,
        TemplateSensor,
        UnitOfMeasurement::{self, Celcius},
        UnitTime,
    },
    package::Customize,
    template::{Template, TemplateExpression},
    types::ComparableNumber,
    Package,
};

#[derive(thiserror::Error, Debug)]
pub enum TranslationError {
    #[allow(unused)]
    #[error("Some kind of error here")]
    Something,
}

const FORCE_UPDATE_DUMMY_ATTRIBUTE: &'static str = "force update dummy";

fn temperature_in_1_hour(
    temperature: EntityMember,
    derivates: &[(EntityMember, Option<f32>)],
) -> Template {
    let mut template = Template::default();
    let members: Box<[_]> = derivates
        .iter()
        .enumerate()
        .map(|(idx, (member, multiplier))| {
            (
                Cow::Owned(format!("d{idx}")),
                member.to_ha_call().to_float(),
                *multiplier,
            )
        })
        .collect();

    let namespace_members: Vec<(Cow<'static, _>, _, _)> = [(
        Cow::Borrowed("t"),
        temperature.to_ha_call().to_float(),
        None,
    )]
    .into_iter()
    .chain(members)
    .collect();
    let namespace = template.assign_new_namespace(
        namespace_members
            .iter()
            .map(|(name, template, _mul)| (name.clone(), template.clone())),
    );
    let variables: Vec<_> = namespace_members
        .into_iter()
        .map(|(name, _, multiplier)| {
            (
                Rc::from(namespace.member(name)),
                multiplier.map(TemplateExpression::literal),
            )
        })
        .collect();
    let iterations = TemplateExpression::literal(12);
    template.for_each(
        TemplateExpression::range(TemplateExpression::literal(0), iterations.clone()),
        |template, _var| {
            for window in variables.windows(2) {
                let [(target, _), (derivate, multiplier)] = window else {
                    unreachable!()
                };
                let mut change = &****derivate / iterations.clone();
                if let Some(multiplier) = multiplier {
                    change = &*change * multiplier.clone();
                }
                template.assign(target, &****target + change);
            }
        },
    );
    template.expr(
        variables
            .into_iter()
            .map(|x| (**x.0).clone())
            .next()
            .expect("Temperature always exist")
            .round(2),
    );
    template
}

fn temperature_template_sensor(name: String, template: impl Into<Template>) -> TemplateSensor {
    TemplateSensor {
        name,
        device_class: Some(Temperature),
        state: template.into(),
        state_class: Some(Measurement),
        unit_of_measurement: Some(Celcius),
        unique_id: None,
        availability: None,
        attributes: Default::default(),
    }
}

struct RoomTemperatureSensors {
    chosen_temperature: EntityMember,
    current_temperature_average: EntityMember,
    external_weight: Option<EntityMember>,
}

impl Package {
    fn build(
        &mut self,
        room_name: &str,
        room: &Room,
        temperature_offset: &EntityMember,
        jitter: Rc<TemplateExpression>,
    ) -> RoomTemperatureSensors {
        let mut room_current_temperature_entities = Vec::with_capacity(room.radiators.len() + 1);
        let mut room_chosen_temperature_entities = Vec::with_capacity(room.radiators.len() + 1);
        let externals = if let Some(external_sensor) = &room.temperature_sensor {
            let external_temperature =
                EntityMember(external_sensor.clone(), EntityMemberType::State);
            room_current_temperature_entities.push(external_temperature.clone());
            let name = format!("{room_name} external temperature weight",);
            let entity = self.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::Percent,
                min: 0.into(),
                max: 1000.into(),
                step: 5.into(),
                icon: Some("mdi:scale-unbalanced".into()),
                mode: InputNumberMode::Box,
            };
            self.customize(entity.clone(), Customize::Initial, 100);
            self.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::PowerFactor,
            );
            self.helpers.insert(entity.clone(), input);
            Some((external_temperature, EntityMember::state(entity)))
        } else {
            None
        };
        for radiator in &room.radiators[..] {
            room_current_temperature_entities.push(EntityMember(
                radiator.entity_id.clone(),
                "current_temperature".into(),
            ));
            room_chosen_temperature_entities.push(EntityMember(
                radiator.entity_id.clone(),
                "temperature".into(),
            ));
        }
        let entity_id = {
            let name = format!("{room_name} current temperature (internal)");
            let entity = self.new_entity_id(TemplateSensor::entity_type(), &name);
            self.customize(entity.clone(), Customize::FriendlyName, name);
            self.customize(entity.clone(), Customize::DisplayPrecision, 2);
            self.customize(
                entity.clone(),
                Customize::DeviceClass,
                serde_yaml::to_value(DeviceClass::Temperature).unwrap(),
            );
            entity
        };
        let mut divider = TemplateExpression::literal(room.radiators.len());
        let mut temperature_sum = TemplateExpression::sum(
            room.radiators
                .iter()
                .map(|x| {
                    EntityMember(x.entity_id.clone(), "current_temperature".into())
                        .to_ha_call()
                        .to_float()
                })
                .collect::<Vec<_>>(),
        )
        .unwrap();
        let mut current_temperature_template = Template::default();
        let mut maybe_external_weight = None;
        if let Some((external_temperature, external_weight)) = externals {
            let weight = &*external_weight
                .to_ha_call_named("external_weight")
                .to_float()
                / TemplateExpression::literal(100);
            maybe_external_weight = Some(external_weight);
            divider = &*divider + weight.clone();
            temperature_sum = &*temperature_sum
                + (&*external_temperature.to_ha_call_named("external").to_float() * weight);
        }
        current_temperature_template.expr(&*(&*temperature_sum / divider) + jitter);
        let mut current_temperature_average_sensor =
            temperature_template_sensor(entity_id.id.to_string(), current_temperature_template);
        current_temperature_average_sensor
            .set_availability_from(&room_current_temperature_entities[..]);
        let current_temperature_average = EntityMember::state(entity_id);
        current_temperature_average_sensor.attributes.insert(
            FORCE_UPDATE_DUMMY_ATTRIBUTE.into(),
            TemplateExpression::now()
                .member(Cow::Borrowed("minute"))
                .into(),
        );
        self.helpers.insert(
            (),
            current_temperature_average_sensor
                .with_trigger(Trigger::TimePattern {
                    hours: TimeInterval::Any,
                    minutes: TimeInterval::Any,
                    seconds: TimeInterval::At(0),
                })
                .with_trigger(Trigger::State {
                    entity_id: room_current_temperature_entities
                        .iter()
                        .map(|x| x.entity_id())
                        .collect(),
                }),
        );
        let chosen_temperature = {
            let temperature_template: Rc<TemplateExpression> =
                &*match &room_chosen_temperature_entities[..] {
                    [single_radiator] => single_radiator.to_ha_call_named("radiator").to_float(),
                    ref multiple => TemplateExpression::max(
                        multiple
                            .iter()
                            .map(|x| x.to_ha_call_named("radiators").to_float()),
                    )
                    .expect("There are at least one radiator"),
                } + temperature_offset.to_ha_call_named("offset").to_float();
            let template_name = format!("{} target temperature", room_name);
            let entity_id = self.new_entity_id(TemplateSensor::entity_type(), &template_name);
            self.customize(entity_id.clone(), Customize::FriendlyName, template_name);
            self.customize(
                entity_id.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            let mut chosen_temperature_sensor =
                temperature_template_sensor(entity_id.id.to_string(), temperature_template);
            chosen_temperature_sensor.set_availability_from(&room_chosen_temperature_entities);
            let chosen_temperature = EntityMember::state(entity_id);
            self.helpers.insert((), chosen_temperature_sensor);
            chosen_temperature
        };
        RoomTemperatureSensors {
            chosen_temperature,
            current_temperature_average,
            external_weight: maybe_external_weight,
        }
    }

    fn create_derivative(
        &mut self,
        room_name: &str,
        level: u8,
        sensor: EntityId,
        derivative_duration: Duration,
    ) -> EntityMember {
        let derivate_level: String = vec!['d'; level as usize + 1].into_iter().collect();
        let derivate_entity = {
            let name = format!("{room_name} temperature {derivate_level}t");
            let entity = self.new_entity_id(DerivativeSensor::entity_type(), &name);
            self.customize(entity.clone(), Customize::FriendlyName, name);
            self.customize(entity.clone(), Customize::Hidden, true);
            self.state
                .groups
                .internal_entities
                .entities
                .push(entity.clone());
            EntityMember::state(entity)
        };
        let temperature_entity = sensor;
        let derivate_sensor = DerivativeSensor {
            name: derivate_entity.entity_id().id.to_string(),
            source: temperature_entity,
            time_window: derivative_duration,
            unit_time: Some(UnitTime::Hours),
            round: None,
        };
        self.helpers.insert((), derivate_sensor);
        let derivate_dummy_entity = {
            let name = format!("{room_name} temperature {derivate_level}t dummy");
            let entity = self.new_entity_id(TemplateSensor::entity_type(), &name);
            self.customize(entity.clone(), Customize::FriendlyName, name);
            self.customize(entity.clone(), Customize::Hidden, true);
            self.state
                .groups
                .internal_entities
                .entities
                .push(entity.clone());
            EntityMember::state(entity)
        };
        let mut derivate_dummy_sensor = temperature_template_sensor(
            derivate_dummy_entity.entity_id().id.to_string(),
            &*derivate_entity.to_ha_call().to_float() - self.jitter(),
        );
        derivate_dummy_sensor.set_availability_from(&[derivate_entity.clone()]);
        derivate_dummy_sensor.unit_of_measurement = None;
        derivate_dummy_sensor.device_class = None;
        derivate_dummy_sensor.attributes.insert(
            FORCE_UPDATE_DUMMY_ATTRIBUTE.into(),
            TemplateExpression::now()
                .member(Cow::Borrowed("minute"))
                .into(),
        );
        self.helpers.insert(
            (),
            derivate_dummy_sensor
                .with_trigger(Trigger::TimePattern {
                    hours: TimeInterval::Any,
                    minutes: TimeInterval::Any,
                    seconds: TimeInterval::At(0),
                })
                .with_trigger(Trigger::State {
                    entity_id: vec![derivate_entity.entity_id()],
                }),
        );
        derivate_dummy_entity
    }
}

impl TryFrom<&ClimateConfig> for Package {
    type Error = TranslationError;

    fn try_from(config: &ClimateConfig) -> Result<Self, Self::Error> {
        let mut output = Self::default();
        output.state.entity_id_prefix = config.entity_id_prefix.clone();
        output.state.jitter = {
            let two = TemplateExpression::literal(2);
            Some(
                &*(&*TemplateExpression::literal(1)
                    - &*(&*TemplateExpression::now().member(Cow::Borrowed("minute"))
                        % two.clone())
                        * two)
                    * TemplateExpression::literal(config.jitter.to_string()),
            )
        };
        let with_prefix = {
            let prefix = config.entity_id_prefix.clone();
            move |name| EntityId::encode_string(&format!("{prefix}_{name}"))
        };
        let radiator_temperature_offset = {
            let name = "Temperature offset for radiators";
            let entity = output.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::Celcius,
                min: 0.into(),
                max: 10.into(),
                step: ComparableNumber::Float(0.5),
                icon: Some("mdi:thermometer-plus".into()),
                mode: InputNumberMode::Box,
            };
            output
                .state
                .groups
                .global_configuration_entities
                .entities
                .push(entity.clone());
            output.customize(entity.clone(), Customize::Initial, 0.5);
            output.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            output.helpers.insert(entity.clone(), input);
            EntityMember::state(entity)
        };
        let full_close_friction = if config.full_close_friction {
            let name = "Extra friction to fully close";
            let entity = output.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::None,
                min: 0.into(),
                max: 20.into(),
                step: ComparableNumber::Int(1),
                icon: Some("mdi:slope-uphill".into()),
                mode: InputNumberMode::Box,
            };
            output
                .state
                .groups
                .global_configuration_entities
                .entities
                .push(entity.clone());
            output.customize(entity.clone(), Customize::Initial, 2);
            output.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::PowerFactor,
            );
            output.helpers.insert(entity.clone(), input);
            Some(EntityMember::state(entity))
        } else {
            None
        };
        let sensitivity = [
            (
                "heat",
                "mdi:thermometer-chevron-up",
                config.acceptable_temperature_difference,
            ),
            (
                "cold",
                "mdi:thermometer-chevron-down",
                config.acceptable_temperature_difference,
            ),
        ]
        .into_iter()
        .map(|(what, icon, initial)| {
            let name = format!("Radiator temperature {what} sensitivity");
            let entity = output.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::Celcius,
                min: ComparableNumber::Float(0.05),
                max: 2.into(),
                step: ComparableNumber::Float(0.05),
                icon: Some(icon.into()),
                mode: InputNumberMode::Box,
            };
            output
                .state
                .groups
                .global_configuration_entities
                .entities
                .push(entity.clone());
            output.customize(entity.clone(), Customize::Initial, initial.unwrap_or(0.75));
            output.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            output.helpers.insert(entity.clone(), input);
            EntityMember::state(entity)
        })
        .collect::<Vec<_>>();
        let Some([heat_sensitivity, cold_sensitivity]) = sensitivity.first_chunk() else {
            unreachable!()
        };
        for (room_name, room) in config.rooms.iter() {
            let never_triggered = Condition::Template {
                value_template: TemplateExpression::this_automation_last_trigger()
                    .is_none()
                    .into(),
            };
            let RoomTemperatureSensors {
                chosen_temperature,
                current_temperature_average: temperature_entity,
                external_weight,
            } = output.build(
                room_name,
                room,
                &radiator_temperature_offset,
                output.jitter(),
            );
            output
                .state
                .groups
                .internal_entities
                .entities
                .push(temperature_entity.entity_id());
            if let Some(external_weight) = external_weight {
                output
                    .state
                    .groups
                    .individual_configuration_entities
                    .entities
                    .push(external_weight.entity_id());
            }
            {
                let name = format!("{room_name} current temperature");
                let entity = output.new_entity_id(TemplateSensor::entity_type(), &name);
                let mut sensor = temperature_template_sensor(
                    entity.id.to_string(),
                    temperature_entity
                        .to_ha_call_named("temp_now")
                        .to_float()
                        .round(1),
                );
                sensor.set_availability_from(&[temperature_entity.clone()]);
                output.customize(entity.clone(), Customize::FriendlyName, name);
                output.customize(entity.clone(), Customize::Hidden, false);
                output.customize(temperature_entity.entity_id(), Customize::Hidden, true);
                output.state.groups.status_entities.entities.push(entity);
                output.helpers.insert((), sensor);
            }
            output
                .state
                .groups
                .status_entities
                .entities
                .push(chosen_temperature.entity_id());
            let derivates_entities: Box<[_]> = {
                let mut last_sensor = temperature_entity.clone();
                room.derivative_spannings
                    .as_ref()
                    .unwrap_or(&config.derivative_spannings)
                    .iter()
                    .enumerate()
                    .map(|(level, derivate)| {
                        let new = output.create_derivative(
                            room_name,
                            level as u8,
                            last_sensor.static_entity_id(),
                            derivate.duration.clone(),
                        );
                        last_sensor = new.clone();
                        (new, derivate.multiplier)
                    })
                    .collect()
            };
            let predicted_temperature_entity = {
                let name = format!("{room_name} calculated temperature in 1 hour");
                let entity = output.new_entity_id(TemplateSensor::entity_type(), &name);
                output
                    .state
                    .groups
                    .status_entities
                    .entities
                    .push(entity.clone());
                output.customize(entity.clone(), Customize::FriendlyName, name);
                output.customize(
                    entity.clone(),
                    Customize::DeviceClass,
                    DeviceClass::Temperature,
                );
                output.customize(entity.clone(), Customize::Icon, "mdi:math-integral");
                output.customize(entity.clone(), Customize::Round, 1);
                EntityMember::state(entity)
            };
            let mut prediction_sensor = temperature_template_sensor(
                predicted_temperature_entity.entity_id().id.to_string(),
                temperature_in_1_hour(temperature_entity.clone(), &derivates_entities),
            );
            let prediction_sources: Box<[_]> = [temperature_entity.clone()]
                .into_iter()
                .chain(derivates_entities.iter().map(|(entity, _)| entity.clone()))
                .collect();
            prediction_sensor.set_availability_from(&prediction_sources);
            let chosen_temperature_value = chosen_temperature
                .to_ha_call_named("chosen_temp")
                .to_float();
            let too_warm = predicted_temperature_entity
                .to_ha_call_named("predicted_temp")
                .to_float()
                .gt(chosen_temperature_value.clone())
                .mark_named_const_expr("too_warm");
            let too_warm_now = temperature_entity
                .to_ha_call()
                .to_float()
                .gt(chosen_temperature_value)
                .mark_named_const_expr("too_warm_now");

            output.helpers.insert(
                (),
                prediction_sensor.with_trigger(Trigger::State {
                    entity_id: vec![
                        prediction_sources.first().unwrap().entity_id(),
                        prediction_sources.last().unwrap().entity_id(),
                    ],
                }),
            );
            for radiator in &room.radiators {
                output.next_iteration();
                let mut extra_run_conditions: Option<Condition> = None;
                let mut add_run_condition = |condition: Condition| {
                    if let Some(conditions) = &mut extra_run_conditions {
                        extra_run_conditions = Some(conditions.clone().or(condition.clone()));
                    } else {
                        extra_run_conditions = Some(condition.clone());
                    }
                    condition
                };
                let mut used_entities = HashSet::new();
                let mut add_automation_entity: Box<dyn FnMut(EntityMember) -> EntityMember> =
                    Box::new(|entity| {
                        HashSet::insert(&mut used_entities, entity.clone());
                        entity
                    });
                let radiator_action = add_automation_entity(EntityMember(
                    radiator.entity_id.clone(),
                    "hvac_action".into(),
                ));
                let closing_percent = EntityMember::state(EntityId::external(
                    EntityType::Number,
                    &format!("{}_valve_closing_degree", radiator.entity_id.id),
                ));
                let closing_percent_value =
                    closing_percent.to_ha_call_named("closing_percent").to_int();
                let fully_closed = EntityMember(closing_percent.entity_id(), "max".into())
                    .to_ha_call_named("fully_closed")
                    .to_int();
                let closing_step = EntityMember(closing_percent.static_entity_id(), "step".into());
                let is_not_heating = Condition::State {
                    entity_id: radiator.entity_id.clone().into(),
                    attribute: Some("hvac_action".into()),
                    state: Some("idle".into()),
                    r#for: None,
                };
                let after_heat_backoff = Condition::State {
                    entity_id: radiator_action.static_entity_id(),
                    attribute: radiator_action.static_attribute(),
                    state: Some("idle".into()),
                    r#for: Some(config.backoff_after_heat.clone()),
                };
                let mut actions = vec![Choose {
                    conditions: vec![never_triggered.clone()],
                    sequence: vec![Action::Stop(Stop {
                        stop: "We don't have any information on first execution".into(),
                        enabled: true,
                    })],
                }];
                let predicted_above_requested_condition =
                    chosen_temperature.clone().smaller_than_entity(
                        predicted_temperature_entity
                            .clone()
                            .state_entity()
                            .expect("Current temperature must be a state"),
                    );
                let comfortable_temperature_multiplier_abs = {
                    let chosen_temperature = add_automation_entity(chosen_temperature.clone())
                        .to_ha_call()
                        .to_float();
                    let predicted_temperature_entity =
                        add_automation_entity(predicted_temperature_entity.clone())
                            .to_ha_call()
                            .to_float();
                    TemplateExpression::if_then_else(
                        too_warm.clone(),
                        &*(&*predicted_temperature_entity - chosen_temperature.clone())
                            / add_automation_entity(heat_sensitivity.clone())
                                .to_ha_call_named("heat_sensitivity")
                                .to_float(),
                        &*(&*chosen_temperature - predicted_temperature_entity)
                            / add_automation_entity(cold_sensitivity.clone())
                                .to_ha_call_named("cold_sensitivity")
                                .to_float(),
                    )
                }
                .mark_named_const_expr("comfortable_temp_multiplier")
                .to_int();
                if room.valve_closing_automation {
                    let min_closing_valve_entity = {
                        let name = format!(
                            "{} min closing valve",
                            radiator.entity_id.assumed_friendly_name()
                        );
                        let entity = output.new_entity_id(InputNumber::entity_type(), &name);
                        let input = InputNumber {
                            name: Some(name.into()),
                            unit_of_measurement: UnitOfMeasurement::Percent,
                            min: 0.into(),
                            max: 100.into(),
                            step: 1.into(),
                            icon: Some("mdi:valve".into()),
                            mode: InputNumberMode::Slider,
                        };
                        output
                            .state
                            .groups
                            .individual_configuration_entities
                            .entities
                            .push(entity.clone());
                        output.customize(
                            entity.clone(),
                            Customize::Initial,
                            config.default_min_closing_percent.unwrap_or(80),
                        );
                        output.customize(
                            entity.clone(),
                            Customize::DeviceClass,
                            DeviceClass::PowerFactor,
                        );
                        output.helpers.insert(entity.clone(), input);
                        EntityMember::state(entity)
                    };
                    let max_closing_valve_entity = {
                        let name = format!(
                            "{} max closing valve",
                            radiator.entity_id.assumed_friendly_name()
                        );
                        let entity = output.new_entity_id(InputNumber::entity_type(), &name);
                        let input = InputNumber {
                            name: Some(name.into()),
                            unit_of_measurement: UnitOfMeasurement::Percent,
                            min: 0.into(),
                            max: 100.into(),
                            step: 1.into(),
                            icon: Some("mdi:valve-closed".into()),
                            mode: InputNumberMode::Slider,
                        };
                        output
                            .state
                            .groups
                            .individual_configuration_entities
                            .entities
                            .push(entity.clone());
                        output.customize(
                            entity.clone(),
                            Customize::Initial,
                            config.default_max_closing_percent.unwrap_or(100),
                        );
                        output.customize(
                            entity.clone(),
                            Customize::DeviceClass,
                            DeviceClass::PowerFactor,
                        );
                        output.helpers.insert(entity.clone(), input);
                        EntityMember::state(entity)
                    };
                    let script_run_interval = {
                        let name = format!(
                            "{} adjustment interval",
                            radiator.entity_id.assumed_friendly_name()
                        );
                        let entity = output.new_entity_id(InputNumber::entity_type(), &name);
                        let input = InputNumber {
                            name: Some(name.into()),
                            unit_of_measurement: UnitOfMeasurement::Minutes,
                            min: 0.into(),
                            max: 120.into(),
                            step: ComparableNumber::Float(0.25),
                            icon: Some("mdi:timer".into()),
                            mode: InputNumberMode::Box,
                        };
                        output
                            .state
                            .groups
                            .individual_configuration_entities
                            .entities
                            .push(entity.clone());
                        output.customize(
                            entity.clone(),
                            Customize::Initial,
                            config
                                .wait_between_adjustments
                                .as_ref()
                                .map(|x| x.to_seconds() / 60)
                                .unwrap_or(600),
                        );
                        output.customize(
                            entity.clone(),
                            Customize::DeviceClass,
                            DeviceClass::Duration,
                        );
                        output.helpers.insert(entity.clone(), input);
                        let time = add_automation_entity(EntityMember::state(entity))
                            .to_ha_call()
                            .to_float();
                        let sixty = TemplateExpression::literal(60);
                        let hours = (&*time / sixty.clone()).to_int();
                        let minutes = (&*time % sixty.clone()).to_int();
                        let seconds = (&*(&*time * sixty.clone()) % sixty).to_int();
                        Condition::from_template(
                            (&*TemplateExpression::now()
                                - TemplateExpression::this_automation_last_trigger())
                            .gt(TemplateExpression::timedelta(hours, minutes, seconds)),
                        )
                    };

                    let invalid_closing_values_automation = {
                        let max_changed: Rc<str> = "max_changed".into();
                        let min_changed: Rc<str> = "min_changed".into();
                        Automation {
                            name: output.new_automation_identifier(&format!(
                                "Fix illegal values for closing valve settings for {}",
                                radiator.entity_id.assumed_friendly_name()
                            )),
                            trace: None,
                            trigger: vec![
                                TriggerHolder {
                                    id: Some(max_changed.clone()),
                                    trigger: Trigger::State {
                                        entity_id: vec![max_closing_valve_entity
                                            .state_entity()
                                            .unwrap()],
                                    },
                                },
                                TriggerHolder {
                                    id: Some(min_changed.clone()),
                                    trigger: Trigger::State {
                                        entity_id: vec![min_closing_valve_entity
                                            .state_entity()
                                            .unwrap()],
                                    },
                                },
                            ],
                            condition: vec![max_closing_valve_entity.clone().smaller_than_entity(
                                min_closing_valve_entity.state_entity().unwrap(),
                            )],
                            actions: vec![vec![
                                Choose {
                                    conditions: vec![Condition::Trigger {
                                        id: max_changed,
                                        enabled: true,
                                    }],
                                    sequence: vec![Service::SetInputNumberValue {
                                        data: Service::template_data(
                                            max_closing_valve_entity.to_ha_call().to_int(),
                                        ),
                                        target: min_closing_valve_entity
                                            .state_entity()
                                            .unwrap()
                                            .into(),
                                    }
                                    .into()],
                                },
                                Choose {
                                    conditions: vec![Condition::Trigger {
                                        id: min_changed,
                                        enabled: true,
                                    }],
                                    sequence: vec![Service::SetInputNumberValue {
                                        data: Service::template_data(
                                            min_closing_valve_entity.to_ha_call().to_int(),
                                        ),
                                        target: max_closing_valve_entity
                                            .state_entity()
                                            .unwrap()
                                            .into(),
                                    }
                                    .into()],
                                },
                            ]
                            .into()],
                        }
                    };
                    output.automation.push(invalid_closing_values_automation);
                    let template_closing_percent = closing_percent_value.clone();
                    let template_closing_step =
                        closing_step.to_ha_call_named("closing_step").to_int();
                    let adjust_steps_abs = (&*comfortable_temperature_multiplier_abs.clone()
                        * template_closing_step.clone())
                    .mark_named_const_expr("adjust_steps_abs");
                    let lit_one = TemplateExpression::literal(1);
                    let adjust_steps_multiplier = TemplateExpression::if_then_else(
                        too_warm.clone(),
                        lit_one.clone(),
                        TemplateExpression::literal(-1),
                    );
                    let adjust_steps = &*adjust_steps_multiplier
                        * (&*lit_one + adjust_steps_abs.clone()).log2().to_int();
                    let may_close = Condition::from_template(
                        (&*template_closing_percent + template_closing_step.clone())
                            .le(max_closing_valve_entity.to_ha_call().to_int())
                            .mark_named_const_expr("may_close"),
                    );
                    let may_open = Condition::from_template(
                        (&*template_closing_percent - template_closing_step.clone())
                            .ge(min_closing_valve_entity.to_ha_call().to_int())
                            .mark_named_const_expr("may_open"),
                    );
                    let should_change = Condition::from_template(
                        adjust_steps_abs.clone().gt(TemplateExpression::literal(0)),
                    );
                    let almost_fully_closed = (&*fully_closed - template_closing_step)
                        .mark_named_const_expr("almost_closed");
                    let new_valve_percent = {
                        let new_closing_percent = (&*template_closing_percent
                            + adjust_steps.clone())
                        .mark_named_const_expr("new_closing");
                        if radiator
                            .full_close_only_at_real_temperature
                            .or(room.full_close_only_at_real_temperature)
                            .or(config.full_close_only_at_real_temperature)
                            .unwrap_or(false)
                        {
                            TemplateExpression::if_then_else(
                                too_warm_now.clone().not().and(
                                    new_closing_percent.clone().gt(almost_fully_closed.clone()),
                                ),
                                almost_fully_closed.clone(),
                                new_closing_percent,
                            )
                        } else {
                            new_closing_percent
                        }
                    };
                    let new_closing = if let Some(friction) =
                        full_close_friction.clone().and_then(|x| {
                            if radiator
                                .full_close_friction
                                .or(room.full_close_friction)
                                .unwrap_or(true)
                            {
                                Some(x)
                            } else {
                                None
                            }
                        }) {
                        let friction = add_automation_entity(friction)
                            .to_ha_call_named("friction")
                            .to_int();
                        let to_almost_fully_closed = template_closing_percent
                            .clone()
                            .lt(almost_fully_closed.clone())
                            .and(new_valve_percent.clone().ge(almost_fully_closed.clone()));
                        let to_fully_closed = template_closing_percent
                            .clone()
                            .lt(fully_closed.clone())
                            .and(new_valve_percent.clone().ge(fully_closed.clone()));
                        let passes_friction = adjust_steps_abs.clone().gt(friction);
                        TemplateExpression::if_then_else(
                            to_almost_fully_closed,
                            almost_fully_closed.clone(),
                            TemplateExpression::if_then_else(
                                to_fully_closed,
                                TemplateExpression::if_then_else(
                                    passes_friction,
                                    fully_closed,
                                    almost_fully_closed,
                                ),
                                new_valve_percent.clone(),
                            ),
                        )
                    } else {
                        new_valve_percent.clone()
                    };
                    let new_closing_ranged = TemplateExpression::min(
                        [
                            new_closing,
                            max_closing_valve_entity
                                .to_ha_call_named("max_closing")
                                .to_int(),
                        ]
                        .into_iter(),
                    )
                    .unwrap();
                    let new_opening_ranged = TemplateExpression::max(
                        [
                            new_valve_percent,
                            min_closing_valve_entity
                                .to_ha_call_named("min_closing")
                                .to_int(),
                        ]
                        .into_iter(),
                    )
                    .unwrap();
                    let would_open = Condition::from_template(
                        new_opening_ranged.clone().ne(closing_percent_value.clone()),
                    );
                    let would_close = Condition::from_template(
                        new_closing_ranged.clone().ne(closing_percent_value.clone()),
                    );
                    let log_previous = |expr: Rc<TemplateExpression>| {
                        Action::log(radiator.entity_id.clone(), |msg| {
                            msg.text("Calculated temperature in 1 hour is ");
                            msg.expr(predicted_temperature_entity.entity_id().to_ha_call_pretty());
                            msg.text(". Valves set to ");
                            msg.expr(expr.clone());
                            msg.text(". (was=");
                            msg.expr(closing_percent.entity_id().to_ha_call_pretty());
                            msg.text(", t=");
                            msg.expr(temperature_entity.entity_id().to_ha_call_pretty());
                            msg.text(", dt=(");
                            for derivate in &derivates_entities {
                                msg.expr(derivate.0.entity_id().to_ha_call_pretty());
                                msg.text(", ");
                            }
                            msg.text("), tt=");
                            msg.expr(chosen_temperature.entity_id().to_ha_call_pretty());
                            msg.text(", steps=");
                            msg.expr(adjust_steps.clone());
                            msg.text(")");
                        })
                    };
                    actions.append(&mut vec![
                        Choose {
                            conditions: vec![Condition::comment("No longer affected by the heat cycle, so check if we should turn the passive heat down."),
                                             add_run_condition(
                                may_close
                                    .and(after_heat_backoff)
                                    .and(should_change.clone())
                                    .and(predicted_above_requested_condition.clone())
                                    .and(would_close)
                            )],
                            sequence: vec![
                                log_previous(new_closing_ranged.clone()),
                                Service::SetNumberValue {
                                data: Service::template_data(new_closing_ranged),
                                target: closing_percent.entity_id().into(),
                            }
                            .into()],
                        },
                        Choose {
                            conditions: vec![Condition::comment("It's too cold, try to turn the heat up."),
                                             add_run_condition(
                                may_open
                                    .and(is_not_heating.clone())
                                    .and(should_change)
                                    .and(predicted_above_requested_condition.not().clone())
                                    .and(would_open)
                            )],
                            sequence: vec![
                                log_previous(new_opening_ranged.clone()),
                                Service::SetNumberValue {
                                data: {
                                    Service::template_data(new_opening_ranged)
                                },
                                target: closing_percent.entity_id().into(),
                            }
                            .into()],
                        },
                    ]);
                    drop(add_automation_entity);
                    let mut conditions = vec![];
                    if let Some(availability_checks) = TemplateExpression::fold(
                        used_entities.into_iter().map(|x| x.to_ha_check()),
                        |x, y| x.and(y),
                    ) {
                        conditions.push(Condition::from_template(availability_checks));
                    }
                    conditions.push(never_triggered.clone().or(
                        if let Some(extra_conditions) = extra_run_conditions {
                            script_run_interval.and(extra_conditions)
                        } else {
                            script_run_interval
                        },
                    ));
                    let automation = Automation {
                        name: output.new_automation_identifier(&format!(
                            "Automatic minimum valve opening for {}",
                            radiator.entity_id.assumed_friendly_name()
                        )),
                        trace: Some(TraceOptions {
                            stored_traces: radiator
                                .stored_traces
                                .or(room.stored_traces)
                                .or(config.stored_traces)
                                .unwrap_or(15),
                        }),
                        trigger: [
                            Trigger::State {
                                entity_id: vec![radiator.entity_id.clone().into()],
                            },
                            Trigger::State {
                                entity_id: vec![temperature_entity
                                    .static_state_entity()
                                    .expect("Temperature entity is a state")
                                    .into()],
                            },
                            Trigger::TimePattern {
                                hours: TimeInterval::Unset,
                                minutes: TimeInterval::EveryNth(5),
                                seconds: TimeInterval::At(
                                    (output.iteration().overflowing_mul(7).0 % 60) as u8,
                                ),
                            },
                        ]
                        .into_iter()
                        .map(Into::into)
                        .collect(),
                        condition: conditions,
                        actions: vec![actions.into()],
                    };
                    output.automation.push(automation);
                }
            }
        }
        output.helpers.group.insert(
            with_prefix("individual_configuration").into(),
            output
                .state
                .groups
                .individual_configuration_entities
                .clone(),
        );
        output.helpers.group.insert(
            with_prefix("global_configuration").into(),
            output.state.groups.global_configuration_entities.clone(),
        );
        output.helpers.group.insert(
            with_prefix("internal_entities").into(),
            output.state.groups.internal_entities.clone(),
        );
        output.helpers.group.insert(
            with_prefix("status_entities").into(),
            output.state.groups.status_entities.clone(),
        );
        Ok(output)
    }
}
