use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
    sync::Mutex,
};

use serde::Serialize;

use crate::{
    automation::Automation,
    entity_id::EntityId,
    helpers::{Helpers, OldStyleGroup},
    template::TemplateExpression,
};

type CustomizeType = HashMap<EntityId, HashMap<Customize, serde_yaml::Value>>;

#[derive(Serialize, Default)]
pub struct HomeAssistant {
    pub customize: CustomizeType,
}

pub struct PackageGroups {
    pub internal_entities: OldStyleGroup,
    pub global_configuration_entities: OldStyleGroup,
    pub status_entities: OldStyleGroup,
    pub individual_configuration_entities: OldStyleGroup,
}

impl Default for PackageGroups {
    fn default() -> Self {
        let internal_entities = OldStyleGroup {
            name: "Radiator Temperature Internals".into(),
            entities: Default::default(),
        };
        let global_configuration_entities = OldStyleGroup {
            name: "Radiator Temperature Global Configuration".into(),
            entities: Default::default(),
        };
        let status_entities = OldStyleGroup {
            name: "Radiator Temperature Status".into(),
            entities: Default::default(),
        };
        let individual_configuration_entities = OldStyleGroup {
            name: "Radiators Temperature Configuration".into(),
            entities: Default::default(),
        };
        Self {
            internal_entities,
            global_configuration_entities,
            status_entities,
            individual_configuration_entities,
        }
    }
}

#[derive(Default)]
pub struct PackageState {
    pub entity_id_prefix: Rc<str>,
    pub known_entity_ids: Mutex<HashSet<EntityId>>,
    pub jitter: Option<Rc<TemplateExpression>>,
    pub groups: PackageGroups,
    iteration: usize,
}

impl Package {
    pub fn next_iteration(&mut self) {
        self.state.iteration += 1
    }

    pub fn iteration(&self) -> usize {
        self.state.iteration
    }

    pub fn jitter(&self) -> Rc<TemplateExpression> {
        self.state
            .jitter
            .clone()
            .unwrap_or_else(|| TemplateExpression::literal(0))
    }
}

#[derive(Serialize, Default)]
pub struct Package {
    pub homeassistant: HomeAssistant,
    #[serde(skip)]
    pub state: PackageState,
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
