use std::{collections::HashSet, ops::Deref as _, rc::Rc};

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
        InputNumber, InputNumberMode,
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

fn temperature_in_1_hour(
    temperature: EntityMember,
    derivate: EntityMember,
    trend: EntityMember,
) -> Template {
    let mut template = Template::default();
    let trend = template.assign_new(trend.to_ha_call().to_float());
    let namespace = template.assign_new_namespace([
        ("t", temperature.to_ha_call().to_float()),
        ("d", derivate.to_ha_call().to_float()),
    ]);
    let temperature_name = namespace.member("t");
    let temperature = temperature_name.deref().clone();
    let derivate_name = namespace.member("d");
    let derivate = derivate_name.deref().clone();
    let iterations = TemplateExpression::literal(12);
    template.for_each(
        TemplateExpression::range(TemplateExpression::literal(0), iterations.clone()),
        |template, _var| {
            template.assign(
                &temperature_name,
                &*temperature + (&*derivate / iterations.clone()),
            );
            template.assign(&derivate_name, &*derivate + (&**trend / iterations))
        },
    );
    template.expr(temperature.round(2));
    template
    //JinjaTemplate(
    //    format!(
    //        "
    //    {{%- set trend = {trend} %}}
    //    {{%- set calc = namespace(temperature = {temperature}, derivate = {derivate}) %}}
    //    {{%- for iteration in range(0, {iterations}) %}}
    //    {{%- set calc.temperature = calc.temperature + calc.derivate / {iterations} %}}
    //    {{%- set calc.derivate = calc.derivate + trend / {iterations} %}}
    //    {{%- endfor %}}
    //    {{{{ calc.temperature | round(2) }}}}",
    //        temperature = temperature.to_ha_call().to_float().as_raw_template(),
    //        derivate = derivate.to_ha_call().to_float().as_raw_template(),
    //        trend = trend.to_ha_call().to_float().as_raw_template(),
    //        iterations = 12
    //    )
    //    .into(),
    //)
}

fn template_sensor(name: String, template: impl Into<Template>) -> TemplateSensor {
    TemplateSensor {
        name,
        device_class: Some(Temperature),
        state: template.into(),
        state_class: Some(Measurement),
        unit_of_measurement: Some(Celcius),
        unique_id: None,
        availability: None,
    }
}

struct RoomTemperatureSensors {
    chosen_temperature: EntityMember,
    current_temperature_average: EntityMember,
}

impl Package {
    fn build(
        &mut self,
        room_name: &str,
        room: &Room,
        temperature_offset: &EntityMember,
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
                mode: Default::default(),
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
            let name = format!("{room_name} current temperature");
            let entity = self.new_entity_id(TemplateSensor::entity_type(), &name);
            self.customize(entity.clone(), Customize::FriendlyName, name);
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
        if let Some((external_temperature, external_weight)) = externals {
            let weight =
                &*external_weight.to_ha_call().to_float() / TemplateExpression::literal(100);
            divider = &*divider + weight.clone();
            temperature_sum =
                &*temperature_sum + (&*external_temperature.to_ha_call().to_float() * weight);
        }
        let mut current_temperature_average_sensor =
            template_sensor(entity_id.id.to_string(), &*temperature_sum / divider);
        current_temperature_average_sensor
            .set_availability_from(&room_current_temperature_entities[..]);
        let current_temperature_average = EntityMember::state(entity_id);
        self.helpers.insert((), current_temperature_average_sensor);
        let chosen_temperature = {
            let temperature_template: Rc<TemplateExpression> =
                &*match &room_chosen_temperature_entities[..] {
                    [single_radiator] => single_radiator.to_ha_call().to_float(),
                    ref multiple => {
                        TemplateExpression::max(multiple.iter().map(|x| x.to_ha_call().to_float()))
                            .expect("There are at least one radiator")
                    }
                } + temperature_offset.to_ha_call().to_float();
            let template_name = format!("{} target temperature", room_name);
            let entity_id = self.new_entity_id(TemplateSensor::entity_type(), &template_name);
            self.customize(entity_id.clone(), Customize::FriendlyName, template_name);
            self.customize(
                entity_id.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            let mut chosen_temperature_sensor =
                template_sensor(entity_id.id.to_string(), temperature_template);
            chosen_temperature_sensor.set_availability_from(&room_chosen_temperature_entities);
            let chosen_temperature = EntityMember::state(entity_id);
            self.helpers.insert((), chosen_temperature_sensor);
            chosen_temperature
        };
        RoomTemperatureSensors {
            chosen_temperature,
            current_temperature_average,
        }
    }
}

impl TryFrom<&ClimateConfig> for Package {
    type Error = TranslationError;

    fn try_from(config: &ClimateConfig) -> Result<Self, Self::Error> {
        let mut output = Self::default();
        output.entity_id_prefix = config.entity_id_prefix.clone();
        let mut iteration: u8 = 0;
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
            output.customize(entity.clone(), Customize::Initial, 0.5);
            output.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            output.helpers.insert(entity.clone(), input);
            EntityMember::state(entity)
        };
        let acceptable_temperature_difference_step = {
            let name = "Radiator temperature precision";
            let entity = output.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::Celcius,
                min: ComparableNumber::Float(0.25),
                max: 2.into(),
                step: ComparableNumber::Float(0.25),
                icon: Some("mdi:tape-measure".into()),
                mode: InputNumberMode::Box,
            };
            output.customize(entity.clone(), Customize::Initial, 0.75);
            output.customize(
                entity.clone(),
                Customize::DeviceClass,
                DeviceClass::Temperature,
            );
            output.helpers.insert(entity.clone(), input);
            EntityMember::state(entity)
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
            } = output.build(room_name, room, &radiator_temperature_offset);
            let derivate_entity = {
                let name = format!("{room_name} temperature change");
                let entity = output.new_entity_id(DerivativeSensor::entity_type(), &name);
                output.customize(entity.clone(), Customize::FriendlyName, name);
                EntityMember::state(entity)
            };
            let temperature_entity = temperature_entity;
            let derivate_sensor = DerivativeSensor {
                name: derivate_entity.entity_id().id.to_string(),
                source: temperature_entity.entity_id(),
                time_window: room
                    .derivative_spanning
                    .as_ref()
                    .unwrap_or(&config.derivative_spanning)
                    .clone(),
                unit_time: Some(UnitTime::Hours),
                round: Some(2),
            };
            let trend_entity = {
                let name = format!("{room_name} temperature trend");
                let entity = output.new_entity_id(DerivativeSensor::entity_type(), &name);
                output.customize(entity.clone(), Customize::FriendlyName, name);
                EntityMember::state(entity)
            };
            let trend_sensor = DerivativeSensor {
                name: trend_entity.entity_id().id.to_string(),
                source: derivate_entity.clone().static_entity_id(),
                time_window: room
                    .trend_spanning
                    .as_ref()
                    .unwrap_or(&config.trend_spanning)
                    .clone(),
                unit_time: Some(UnitTime::Hours),
                round: Some(2),
            };
            let predicted_temperature_entity = {
                let name = format!("{room_name} calculated temperature in 1 hour");
                let entity = output.new_entity_id(TemplateSensor::entity_type(), &name);
                output.customize(entity.clone(), Customize::FriendlyName, name);
                output.customize(
                    entity.clone(),
                    Customize::DeviceClass,
                    DeviceClass::Temperature,
                );
                output.customize(entity.clone(), Customize::Icon, "mdi:math-integral");
                EntityMember::state(entity)
            };
            let mut prediction_sensor = template_sensor(
                predicted_temperature_entity.entity_id().id.to_string(),
                temperature_in_1_hour(
                    temperature_entity.clone(),
                    derivate_entity.clone(),
                    trend_entity.clone(),
                ),
            );
            prediction_sensor.set_availability_from(&[
                temperature_entity.clone(),
                derivate_entity.clone(),
                trend_entity.clone(),
            ]);

            output.helpers.insert((), prediction_sensor);
            output.helpers.insert((), derivate_sensor);
            output.helpers.insert((), trend_sensor);
            for radiator in &room.radiators {
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
                iteration = iteration.overflowing_add(7).0;
                let radiator_action = add_automation_entity(EntityMember(
                    radiator.entity_id.clone(),
                    "hvac_action".into(),
                ));
                let closing_percent = EntityMember::state(EntityId::external(
                    EntityType::Number,
                    &format!("{}_valve_closing_degree", radiator.entity_id.id),
                ));
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
                let predicted_above_requested = chosen_temperature.clone().smaller_than_entity(
                    predicted_temperature_entity
                        .clone()
                        .state_entity()
                        .expect("Current temperature must be a state"),
                );
                let comfortable_temperature_multiplier = {
                    let chosen_temperature = add_automation_entity(chosen_temperature.clone())
                        .to_ha_call()
                        .to_float();
                    let predicted_temperature_entity =
                        add_automation_entity(predicted_temperature_entity.clone())
                            .to_ha_call()
                            .to_float();
                    &*(&*chosen_temperature - predicted_temperature_entity).abs()
                        / room
                            .acceptable_temperature_difference
                            .or(config.acceptable_temperature_difference)
                            .map(TemplateExpression::literal)
                            .unwrap_or(acceptable_temperature_difference_step.to_ha_call().to_float())
                }
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
                            mode: Default::default(),
                        };
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
                            mode: Default::default(),
                        };
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
                        output.customize(
                            entity.clone(),
                            Customize::Initial,
                            config.wait_between_adjustments.to_seconds(),
                        );
                        output.customize(
                            entity.clone(),
                            Customize::DeviceClass,
                            DeviceClass::Duration,
                        );
                        output.helpers.insert(entity.clone(), input);
                        let time = EntityMember::state(entity).to_ha_call().to_float();
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
                                        entity_id: max_closing_valve_entity.state_entity().unwrap(),
                                    },
                                },
                                TriggerHolder {
                                    id: Some(min_changed.clone()),
                                    trigger: Trigger::State {
                                        entity_id: min_closing_valve_entity.state_entity().unwrap(),
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
                    let template_closing_percent = closing_percent.to_ha_call().to_int();
                    let template_closing_step = closing_step.to_ha_call().to_int();
                    let mut adjust_steps =
                        (&*comfortable_temperature_multiplier * template_closing_step.clone()).mark_const_expr();
                    if let Some(friction) = room.full_close_friction.or(config.full_close_friction)
                    {
                        let friction = TemplateExpression::literal(friction);
                        adjust_steps = TemplateExpression::if_then_else(
                            (&*template_closing_percent + adjust_steps.clone())
                                .le(&*max_closing_valve_entity.to_ha_call().to_int()
                                    - friction.clone()),
                            adjust_steps.clone(),
                            &*adjust_steps - friction,
                        );
                    }
                    let may_close = Condition::from_template(
                        (&*template_closing_percent + template_closing_step.clone())
                            .le(max_closing_valve_entity.to_ha_call().to_int()),
                    );
                    let may_open = Condition::from_template(
                        (&*template_closing_percent - template_closing_step)
                            .ge(min_closing_valve_entity.to_ha_call().to_int()),
                    );
                    let should_change = Condition::from_template(
                        adjust_steps.clone().gt(TemplateExpression::literal(0)),
                    );
                    actions.append(&mut vec![
                        Choose {
                            conditions: vec![Condition::comment("No longer affected by the heat cycle, so check if we should turn the passive heat down."),
                                             add_run_condition(
                                may_close
                                    .and(after_heat_backoff)
                                    .and(should_change.clone())
                                    .and(predicted_above_requested.clone()),
                            )],
                            sequence: vec![Service::SetNumberValue {
                                data: Service::template_data(TemplateExpression::min([&*template_closing_percent + adjust_steps.clone(), max_closing_valve_entity.to_ha_call().to_int()].into_iter()).unwrap()),
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
                                    .and(predicted_above_requested.not().clone()),
                            )],
                            sequence: vec![Service::SetNumberValue {
                                data: {
                                    Service::template_data(TemplateExpression::max([&*template_closing_percent - adjust_steps, min_closing_valve_entity.to_ha_call().to_int()].into_iter()).unwrap())
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
                        trace: Some(TraceOptions { stored_traces: 15 }),
                        trigger: [
                            Trigger::State {
                                entity_id: radiator.entity_id.clone().into(),
                            },
                            Trigger::State {
                                entity_id: temperature_entity
                                    .static_state_entity()
                                    .expect("Temperature entity is a state")
                                    .into(),
                            },
                            Trigger::TimePattern {
                                hours: TimeInterval::Unset,
                                minutes: TimeInterval::EveryNth(5),
                                seconds: TimeInterval::At(iteration % 60),
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
        Ok(output)
    }
}
