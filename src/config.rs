use std::{collections::HashMap, rc::Rc};

use serde::Deserialize;

use crate::{entity_id::EntityId, helpers::Duration};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RadiatorConfig {
    pub entity_id: EntityId,
    #[serde(default)]
    pub stored_traces: Option<u8>,
    #[serde(default)]
    pub full_close_friction: Option<bool>,
    #[serde(default)]
    pub full_close_only_at_real_temperature: Option<bool>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DerivativeOptions {
    #[serde(default)]
    pub multiplier: Option<f32>,
    #[serde(flatten)]
    pub duration: Duration,
}

#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct Room {
    pub temperature_sensor: Option<EntityId>,
    pub radiators: Vec<RadiatorConfig>,
    #[serde(default)]
    pub valve_closing_automation: bool,
    #[serde(default)]
    pub derivative_spannings: Option<Vec<DerivativeOptions>>,
    #[serde(default)]
    pub full_close_friction: Option<bool>,
    #[serde(default)]
    pub full_close_only_at_real_temperature: Option<bool>,
    #[serde(default)]
    pub stored_traces: Option<u8>,
}

fn default_jitter() -> f32 {
    0.001
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
    pub derivative_spannings: Vec<DerivativeOptions>,
    #[serde(default)]
    pub full_close_friction: bool,
    #[serde(default)]
    pub full_close_only_at_real_temperature: Option<bool>,
    pub backoff_after_heat: Duration,
    pub wait_between_adjustments: Option<Duration>,
    #[serde(default)]
    pub stored_traces: Option<u8>,
    #[serde(default = "default_jitter")]
    pub jitter: f32,
}
