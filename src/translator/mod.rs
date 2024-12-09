use std::{borrow::Cow, collections::HashSet};

use crate::{
    automation::{
        building_blocks::Choose, Action, Automation, Condition, Service, Stop, TimeInterval,
        TraceOptions, Trigger, TriggerHolder,
    },
    config::{ClimateConfig, Room, DEFAULT_ACCEPTABLE_TEMPERATURE_DIFFERENCE},
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
    template::{IntoJinja, JinjaTemplate, TemplateExpression},
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
    temperature: EntityMember<'_>,
    derivate: EntityMember<'_>,
    trend: EntityMember<'_>,
) -> JinjaTemplate<'static> {
    JinjaTemplate(
        format!(
            "
        {{%- set trend = {trend} %}}
        {{%- set calc = namespace(temperature = {temperature}, derivate = {derivate}) %}}
        {{%- for iteration in range(0, {iterations}) %}}
        {{%- set calc.temperature = calc.temperature + calc.derivate / {iterations} %}}
        {{%- set calc.derivate = calc.derivate + trend / {iterations} %}}
        {{%- endfor %}}
        {{{{ calc.temperature | round(2) }}}}",
            temperature = temperature.to_ha_call().to_float().as_raw_template(),
            derivate = derivate.to_ha_call().to_float().as_raw_template(),
            trend = trend.to_ha_call().to_float().as_raw_template(),
            iterations = 12
        )
        .into(),
    )
}

fn template_sensor(name: String, template: impl IntoJinja) -> TemplateSensor {
    TemplateSensor {
        name,
        device_class: Some(Temperature),
        state: template.to_jinja().to_string(),
        state_class: Some(Measurement),
        unit_of_measurement: Some(Celcius),
        unique_id: None,
        availability: None,
    }
}

struct RoomTemperatureSensors {
    chosen_temperature: EntityMember<'static>,
    current_temperature_average: EntityMember<'static>,
}

impl<'a: 'b, 'b> Package<'b> {
    fn build(
        &mut self,
        room_name: &str,
        room: &Room<'_>,
        temperature_offset: &EntityMember,
    ) -> RoomTemperatureSensors {
        let mut room_current_temperature_entities = Vec::with_capacity(room.radiators.len() + 1);
        let mut room_chosen_temperature_entities = Vec::with_capacity(room.radiators.len() + 1);
        let externals = if let Some(external_sensor) = &room.temperature_sensor {
            let external_temperature =
                EntityMember(external_sensor.borrow(), EntityMemberType::State);
            room_current_temperature_entities.push(external_temperature.clone());
            let name = format!("{room_name} external temperature weight",);
            let entity = self.new_entity_id(InputNumber::entity_type(), &name);
            let input = InputNumber {
                name: Some(name.into()),
                unit_of_measurement: UnitOfMeasurement::Percent,
                min: 0.into(),
                max: 1000.into(),
                step: 5.into(),
                icon: Some(Cow::Borrowed("mdi:scale-unbalanced")),
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
                radiator.entity_id.clone().into_static(),
                "current_temperature".into(),
            ));
            room_chosen_temperature_entities.push(EntityMember(
                radiator.entity_id.clone().into_static(),
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
        let mut divider = TemplateExpression::constant(room.radiators.len());
        let mut temperature_sum = TemplateExpression::fold(
            room.radiators.iter().map(|x| {
                EntityMember(
                    x.entity_id.clone().into_static(),
                    "current_temperature".into(),
                )
                .to_ha_call()
                .to_float()
            }),
            |x, y| &x + &y,
        )
        .unwrap();
        if let Some((external_temperature, external_weight)) = externals {
            let weight = &external_weight.to_ha_call().to_float() / 100;
            divider = &divider + &weight;
            temperature_sum =
                &temperature_sum + &(&external_temperature.to_ha_call().to_float() * &weight);
        }
        let mut current_temperature_average_sensor =
            template_sensor(entity_id.id.to_string(), &temperature_sum / &divider);
        current_temperature_average_sensor
            .set_availability_from(&room_current_temperature_entities[..]);
        let current_temperature_average = EntityMember::state(entity_id);
        self.helpers.insert((), current_temperature_average_sensor);

        let chosen_temperature = {
            let temperature_template: TemplateExpression =
                &match &room_chosen_temperature_entities[..] {
                    [single_radiator] => single_radiator.to_ha_call().to_float(),
                    ref multiple => {
                        TemplateExpression::max(multiple.iter().map(|x| x.to_ha_call().to_float()))
                            .expect("There are at least one radiator")
                    }
                } + &temperature_offset.to_ha_call().to_float();
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

impl<'a, 'b: 'a, 'c: 'b> TryFrom<&'b ClimateConfig<'c>> for Package<'a> {
    type Error = TranslationError;

    fn try_from(config: &'b ClimateConfig<'c>) -> Result<Self, Self::Error> {
        let mut output = Self::default();
        output.entity_id_prefix = Cow::Borrowed(config.entity_id_prefix.as_ref());
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
                icon: Some(Cow::Borrowed("mdi:thermometer-plus")),
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
        for (room_name, room) in config.rooms.iter() {
            let never_triggered = Condition::Template {
                value_template: Cow::Borrowed("{{ this.attributes.last_triggered is none }}"),
            };
            let RoomTemperatureSensors {
                chosen_temperature,
                current_temperature_average: temperature_entity,
            } = output.build(room_name, room, &radiator_temperature_offset);
            let derivate_entity = EntityMember::state(output.new_entity_id(
                DerivativeSensor::entity_type(),
                &format!("{room_name} temperature change"),
            ));
            let temperature_entity = temperature_entity.into_static();
            let derivate_sensor = DerivativeSensor {
                name: derivate_entity.entity_id().id.to_string(),
                source: temperature_entity.entity_id().into_static(),
                time_window: room
                    .derivative_spanning
                    .as_ref()
                    .unwrap_or(&config.derivative_spanning)
                    .clone(),
                unit_time: Some(UnitTime::Hours),
                round: Some(2),
            };
            let trend_entity = EntityMember::state(output.new_entity_id(
                DerivativeSensor::entity_type(),
                &format!("{room_name} temperature trend"),
            ));
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
                let mut extra_run_conditions: Option<Condition<'static>> = None;
                let mut add_run_condition = |condition: Condition<'static>| {
                    if let Some(conditions) = &mut extra_run_conditions {
                        extra_run_conditions = Some(conditions.clone().or(condition.clone()));
                    } else {
                        extra_run_conditions = Some(condition.clone());
                    }
                    condition
                };
                let mut used_entities = HashSet::new();
                let mut add_automation_entity: Box<
                    dyn for<'x> FnMut(EntityMember<'x>) -> EntityMember<'x>,
                > = Box::new(|entity| {
                    HashSet::insert(&mut used_entities, entity.clone().into_static());
                    entity
                });
                iteration = iteration.overflowing_add(7).0;
                let radiator_action = add_automation_entity(EntityMember(
                    radiator.entity_id.clone().into_static(),
                    "hvac_action".into(),
                ));
                let closing_percent = EntityMember::state(EntityId::external(
                    EntityType::Number,
                    format!(
                        "{}_valve_closing_degree",
                        radiator.entity_id.clone().into_static().id
                    )
                    .into(),
                ));
                let closing_step = EntityMember(closing_percent.static_entity_id(), "step".into());
                let is_not_heating = Condition::State {
                    entity_id: radiator.entity_id.clone().into_static().into(),
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
                        stop: Cow::Borrowed("We don't have any information on first execution"),
                        enabled: true,
                    })],
                }];
                let predicted_above_requested = chosen_temperature.clone().smaller_than_entity(
                    predicted_temperature_entity
                        .clone()
                        .state_entity()
                        .expect("Current temperature must be a state"),
                );
                let predicted_comfortable_temeprature = {
                    let chosen_temperature = add_automation_entity(chosen_temperature.clone())
                        .to_ha_call()
                        .to_float();
                    let predicted_temperature_entity =
                        add_automation_entity(predicted_temperature_entity.clone())
                            .to_ha_call()
                            .to_float();
                    Condition::Template {
                        value_template: (&chosen_temperature - &predicted_temperature_entity)
                            .abs()
                            .lt(&TemplateExpression::constant(
                                room.acceptable_temperature_difference
                                    .or(config.acceptable_temperature_difference)
                                    .unwrap_or(DEFAULT_ACCEPTABLE_TEMPERATURE_DIFFERENCE),
                            ))
                            .to_jinja(),
                    }
                };
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
                            icon: Some(Cow::Borrowed("mdi:valve")),
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
                            icon: Some(Cow::Borrowed("mdi:valve-closed")),
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
                            icon: Some(Cow::Borrowed("mdi:timer")),
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
                        let hours = (&time / 60).to_int();
                        let minutes = (&time % 60).to_int();
                        let seconds = (&(&time * 60) % 60).to_int();
                        (&TemplateExpression::now()
                            - &TemplateExpression::this_automation_last_trigger())
                            .gt(&TemplateExpression::timedelta(hours, minutes, seconds))
                            .to_condition()
                    };

                    let invalid_closing_values_automation = {
                        let max_changed: Cow<_> = "max_changed".into();
                        let min_changed: Cow<_> = "min_changed".into();
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
                                        data: max_closing_valve_entity.to_ha_call().to_int().into(),
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
                                        data: min_closing_valve_entity.to_ha_call().to_int().into(),
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
                    let may_close = (&template_closing_percent + &template_closing_step)
                        .le(&max_closing_valve_entity.to_ha_call().to_int())
                        .to_condition();
                    let may_open = (&template_closing_percent - &template_closing_step)
                        .ge(&min_closing_valve_entity.to_ha_call().to_int())
                        .to_condition();
                    actions.append(&mut vec![
                        Choose {
                            conditions: vec![Condition::comment("No longer affected by the heat cycle, so check if we should turn the passive heat down.").and(
                                             add_run_condition(
                                may_close
                                    .and(after_heat_backoff)
                                    .and(predicted_comfortable_temeprature.clone().not())
                                    .and(predicted_above_requested.clone())),
                            )],
                            sequence: vec![Service::SetNumberValue {
                                data: (&template_closing_percent + &template_closing_step).into(),
                                target: closing_percent.entity_id().into_static().into(),
                            }
                            .into()],
                        },
                        Choose {
                            conditions: vec![Condition::comment("It's too cold, try to turn the heat up.").and(
                                             add_run_condition(
                                may_open
                                    .and(is_not_heating.clone())
                                    .and(predicted_comfortable_temeprature.not())
                                    .and(predicted_above_requested.not().clone())),
                            )],
                            sequence: vec![Service::SetNumberValue {
                                data: {
                                    let closing_percent = closing_percent.to_ha_call().to_int();
                                    let closing_step = closing_step.to_ha_call().to_int();
                                    (&closing_percent - &closing_step).into()
                                },
                                target: closing_percent.entity_id().into_static().into(),
                            }
                            .into()],
                        },
                    ]);
                    drop(add_automation_entity);
                    let mut conditions = vec![];
                    if let Some(availability_checks) = TemplateExpression::fold(
                        used_entities.into_iter().map(|x| x.to_ha_check()),
                        |x, y| x.and(&y),
                    ) {
                        conditions.push((&availability_checks).into());
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
                            radiator.entity_id
                        )),
                        trace: Some(TraceOptions { stored_traces: 15 }),
                        trigger: [
                            Trigger::State {
                                entity_id: radiator.entity_id.clone().into_static().into(),
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
