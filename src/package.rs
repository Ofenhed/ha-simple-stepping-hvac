use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Mutex,
};

use serde::Serialize;

use crate::{automation::Automation, entity_id::EntityId, helpers::Helpers};

type CustomizeType = HashMap<EntityId, HashMap<Customize, serde_yaml::Value>>;

#[derive(Serialize, Default)]
pub struct HomeAssistant {
    pub customize: CustomizeType,
}

#[derive(Serialize, Default)]
pub struct Package {
    pub homeassistant: HomeAssistant,
    #[serde(skip)]
    pub entity_id_prefix: Rc<str>,
    #[serde(skip)]
    pub known_entity_ids: Mutex<HashSet<EntityId>>,
    pub automation: Vec<Automation>,
    #[serde(flatten)]
    pub helpers: Helpers,
}

#[derive(Serialize, Clone, Copy, Eq, PartialEq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum Customize {
    DeviceClass,
    FriendlyName,
    Hidden,
    Icon,
    Initial,
    DisplayPrecision,
    Round,
}

impl Package {
    pub fn customize(&mut self, entity: EntityId, key: Customize, value: impl Serialize) -> bool {
        self.homeassistant
            .customize
            .entry(entity)
            .or_default()
            .insert(
                key,
                serde_yaml::to_value(value).expect("All values will be representable"),
            )
            .is_none()
    }
}
