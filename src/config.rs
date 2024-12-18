use std::{collections::HashMap, rc::Rc};

use serde::Deserialize;

use crate::{entity_id::EntityId, helpers::Duration};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RadiatorConfig {
    pub entity_id: EntityId,
    #[serde(default)]
    pub stored_traces: Option<u8>,
}

#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct Room {
    pub temperature_sensor: Option<EntityId>,
    pub radiators: Vec<RadiatorConfig>,
    #[serde(default)]
    pub valve_closing_automation: bool,
    #[serde(default)]
    pub derivative_spanning: Option<Duration>,
    #[serde(default)]
    pub trend_spanning: Option<Duration>,
    //#[serde(default)]
    //pub acceptable_temperature_difference: Option<f32>,
    #[serde(default)]
    pub full_close_friction: Option<bool>,
    #[serde(default)]
    pub stored_traces: Option<u8>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ClimateConfig {
    pub entity_id_prefix: Rc<str>,
    pub rooms: HashMap<Rc<str>, Room>,
    #[serde(default)]
    pub acceptable_temperature_difference: Option<f32>,
    #[serde(default)]
    pub default_min_closing_percent: Option<u8>,
    #[serde(default)]
    pub default_max_closing_percent: Option<u8>,
    pub derivative_spanning: Duration,
    #[serde(default)]
    pub full_close_friction: bool,
    pub trend_spanning: Duration,
    pub backoff_after_heat: Duration,
    pub wait_between_adjustments: Option<Duration>,
    #[serde(default)]
    pub stored_traces: Option<u8>,
}
