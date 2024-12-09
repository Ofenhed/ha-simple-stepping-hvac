use std::{borrow::Cow, collections::HashMap};

use serde::Deserialize;

use crate::{entity_id::EntityId, helpers::Duration};

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RadiatorConfig<'a> {
    #[serde(borrow)]
    pub entity_id: EntityId<'a>,
}

pub const DEFAULT_ACCEPTABLE_TEMPERATURE_DIFFERENCE: f32 = 0.75;

#[derive(Deserialize, Default)]
#[serde(deny_unknown_fields)]
pub struct Room<'a> {
    #[serde(borrow)]
    pub temperature_sensor: Option<EntityId<'a>>,
    #[serde(borrow)]
    pub radiators: Vec<RadiatorConfig<'a>>,
    #[serde(default)]
    pub valve_closing_automation: bool,
    #[serde(default)]
    pub derivative_spanning: Option<Duration>,
    #[serde(default)]
    pub trend_spanning: Option<Duration>,
    #[serde(default)]
    pub acceptable_temperature_difference: Option<f32>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ClimateConfig<'a> {
    pub entity_id_prefix: Cow<'a, str>,
    #[serde(borrow)]
    pub rooms: HashMap<Cow<'a, str>, Room<'a>>,
    #[serde(default)]
    pub acceptable_temperature_difference: Option<f32>,
    #[serde(default)]
    pub default_min_closing_percent: Option<u8>,
    #[serde(default)]
    pub default_max_closing_percent: Option<u8>,
    pub derivative_spanning: Duration,
    pub trend_spanning: Duration,
    pub backoff_after_heat: Duration,
    pub wait_between_adjustments: Duration,
}
