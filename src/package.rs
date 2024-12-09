use std::{borrow::Cow, collections::{HashMap, HashSet}, sync::Mutex};

use serde::Serialize;

use crate::{automation::Automation, entity_id::EntityId, helpers::{Helpers, InputNumber}, types::{ComparableNumber, CustomizationFor}};

#[derive(Serialize)]
pub struct InputNumberCustomization {
    pub initial: ComparableNumber<2, i32>,
}

impl CustomizationFor for InputNumberCustomization {
    type Target = InputNumber<'static>;
}

#[derive(Serialize, Default)]
pub struct Customizations<'a> {
    pub input_number: HashMap<Cow<'a, str>, InputNumberCustomization>,
}

type CustomizeType<'a> = HashMap<EntityId<'a>, HashMap<Customize, serde_yaml::Value>>;

#[derive(Serialize, Default)]
pub struct HomeAssistant<'a> {
    pub customize: CustomizeType<'a>,
}

#[derive(Serialize, Default)]
pub struct Package<'a> {
    pub homeassistant: HomeAssistant<'a>,
    #[serde(skip)]
    pub entity_id_prefix: Cow<'a, str>,
    #[serde(skip)]
    pub known_entity_ids: Mutex<HashSet<EntityId<'a>>>,
    pub automation: Vec<Automation<'a>>,
    #[serde(flatten)]
    pub helpers: Helpers<'a>,
}

#[derive(Serialize, Clone, Copy, Eq, PartialEq, Hash)]
#[serde(rename_all = "snake_case")]
pub enum Customize {
    FriendlyName,
    DeviceClass,
    Initial,
    Icon,
}

impl<'a> Package<'a> {
    pub fn customize(
        &mut self,
        entity: EntityId<'a>,
        key: Customize,
        value: impl Serialize,
    ) -> bool {
        self.homeassistant.customize.entry(entity).or_default().insert(key, serde_yaml::to_value(value).expect("All values will be representable")).is_none()
    }
}
