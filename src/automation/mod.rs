pub mod building_blocks;

use std::{fmt::Display, rc::Rc};

use building_blocks::ConditionalActions;
use serde::Serialize;

use crate::{
    entity_id::{EntityId, EntityMember, EntityType, HasEntityType},
    helpers::Duration,
    template::{Template, TemplateExpression},
    types::{to_string_serialize, ComparableNumber},
    Package,
};

#[derive(Default, PartialEq, Eq, Clone, Copy)]
pub enum TimeInterval {
    At(u8),
    #[allow(unused)]
    EveryNth(u8),
    #[allow(unused)]
    Any,
    #[default]
    Unset,
}

impl TimeInterval {
    fn is_none(&self) -> bool {
        matches!(self, Self::Unset)
    }
}

impl Display for TimeInterval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::At(time) => f.write_str(&time.to_string()),
            Self::Any => f.write_str("*"),
            Self::EveryNth(time) => f.write_fmt(format_args!("/{}", time)),
            Self::Unset => Ok(()),
        }
    }
}

#[derive(Serialize, PartialEq, Eq, Clone)]
#[serde(tag = "trigger", rename_all = "snake_case")]
pub enum Trigger {
    State {
        entity_id: Vec<EntityId>,
        #[serde(skip_serializing_if = "Option::is_none")]
        attribute: Option<Rc<str>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        r#for: Option<Duration>,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<Rc<str>>,
    },
    TimePattern {
        #[serde(
            serialize_with = "to_string_serialize",
            skip_serializing_if = "TimeInterval::is_none"
        )]
        hours: TimeInterval,
        #[serde(
            serialize_with = "to_string_serialize",
            skip_serializing_if = "TimeInterval::is_none"
        )]
        minutes: TimeInterval,
        #[serde(
            serialize_with = "to_string_serialize",
            skip_serializing_if = "TimeInterval::is_none"
        )]
        seconds: TimeInterval,
    },
    Template {
        value_template: Template,
    },
}

#[derive(Serialize)]
pub struct TriggerHolder {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Rc<str>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub r#for: Option<Duration>,
    #[serde(flatten)]
    pub trigger: Trigger,
}

impl From<Trigger> for TriggerHolder {
    fn from(value: Trigger) -> Self {
        Self {
            id: None,
            trigger: value,
            r#for: None,
        }
    }
}

#[derive(Serialize, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum NumericStateComparison {
    #[allow(unused)]
    Above(ComparableNumber<2, i32>),
    #[allow(unused)]
    Below(ComparableNumber<2, i32>),
    #[allow(unused)]
    #[serde(rename = "above")]
    AboveEntityState(EntityId),
    #[allow(unused)]
    #[serde(rename = "below")]
    BelowEntityState(EntityId),
}

impl EntityMember {
    #[allow(unused)]
    pub fn bigger_than(self, other: f32) -> Condition {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::Above(ComparableNumber::Float(other)),
        }
    }
    #[allow(unused)]
    pub fn smaller_than(self, other: f32) -> Condition {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::Below(ComparableNumber::Float(other)),
        }
    }
    #[allow(unused)]
    pub fn bigger_than_entity(self, other: EntityId) -> Condition {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::AboveEntityState(other),
        }
    }
    #[allow(unused)]
    pub fn smaller_than_entity(self, other: EntityId) -> Condition {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::BelowEntityState(other),
        }
    }
}

#[derive(Serialize, Clone)]
#[serde(tag = "condition", rename_all = "snake_case")]
pub enum Condition {
    #[allow(unused)]
    State {
        entity_id: EntityId,
        #[serde(skip_serializing_if = "Option::is_none")]
        attribute: Option<Rc<str>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        r#for: Option<Duration>,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<Rc<str>>,
    },
    #[allow(unused)]
    NumericState {
        entity_id: EntityId,
        #[serde(skip_serializing_if = "Option::is_none")]
        attribute: Option<Rc<str>>,
        #[serde(flatten)]
        compared: NumericStateComparison,
    },
    Trigger {
        id: Rc<str>,
        enabled: bool,
    },
    Template {
        value_template: Template,
    },
    #[allow(unused)]
    Or {
        conditions: Vec<Condition>,
    },
    #[allow(unused)]
    And {
        conditions: Vec<Condition>,
    },
    #[allow(unused)]
    Not {
        conditions: Vec<Condition>,
    },
}

impl Trigger {
    pub fn from_template(template: impl Into<Template>) -> Self {
        Self::Template {
            value_template: template.into(),
        }
    }
}
impl Condition {
    pub fn comment(text: impl Into<Rc<str>>) -> Self {
        Self::Trigger {
            id: text.into(),
            enabled: false,
        }
    }
    pub fn from_template(template: impl Into<Template>) -> Self {
        Self::Template {
            value_template: template.into(),
        }
    }
}
impl Condition {
    pub fn not(self) -> Self {
        Self::Not {
            conditions: vec![self],
        }
    }

    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::And { mut conditions },
                Self::And {
                    conditions: mut other,
                },
            ) => {
                conditions.append(&mut other);
                Self::And { conditions }
            }
            (Self::And { mut conditions }, other) => {
                conditions.push(other);
                Self::And { conditions }
            }
            (other, Self::And { mut conditions }) => {
                let mut new = Vec::with_capacity(conditions.len() + 1);
                new.push(other);
                new.append(&mut conditions);
                Self::And { conditions: new }
            }
            (first, second) => Self::And {
                conditions: vec![first, second],
            },
        }
    }

    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Or { mut conditions },
                Self::Or {
                    conditions: mut other,
                },
            ) => {
                conditions.append(&mut other);
                Self::Or { conditions }
            }
            (Self::Or { mut conditions }, other) => {
                conditions.push(other);
                Self::Or { conditions }
            }
            (other, Self::Or { mut conditions }) => {
                let mut new = Vec::with_capacity(conditions.len() + 1);
                new.push(other);
                new.append(&mut conditions);
                Self::Or { conditions: new }
            }
            (first, second) => Self::Or {
                conditions: vec![first, second],
            },
        }
    }
}

#[derive(Serialize, Clone)]
pub struct Value<T> {
    value: T,
}

impl<T> From<T> for Value<T> {
    fn from(value: T) -> Self {
        Self { value }
    }
}

#[derive(Serialize, Clone)]
#[serde(untagged)]
pub enum TemplatableValue<T> {
    #[allow(unused)]
    Value(T),
    #[allow(unused)]
    Template(Template),
}

impl<T, I: Into<Template>> From<I> for TemplatableValue<T> {
    fn from(value: I) -> Self {
        Self::Template(value.into().into())
    }
}

impl<T, I: Into<Rc<TemplateExpression>>> From<I> for ServiceData<TemplatableValue<T>> {
    fn from(value: I) -> Self {
        Self {
            value: TemplatableValue::Template(value.into().into()),
        }
    }
}

#[derive(Serialize, Clone)]
pub struct ActionTarget {
    pub entity_id: EntityId,
}

impl From<EntityId> for ActionTarget {
    fn from(value: EntityId) -> Self {
        Self { entity_id: value }
    }
}

#[derive(Serialize, Clone)]
pub struct ServiceData<T> {
    pub value: T,
}

impl<T> From<T> for ServiceData<T> {
    fn from(value: T) -> Self {
        ServiceData { value }
    }
}

impl Service {
    pub fn template_data(data: impl Into<Template>) -> ServiceData<TemplatableValue<Value<usize>>> {
        ServiceData {
            value: TemplatableValue::Template(data.into()),
        }
    }
}

#[derive(Serialize, Clone)]
pub struct LogData {
    pub name: Rc<str>,
    pub message: TemplatableValue<&'static str>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub entity_id: Option<EntityId>,
}

impl Action {
    #[must_use]
    pub fn log(entity_id: EntityId, callback: impl Fn(&mut Template)) -> Action {
        let mut template = Default::default();
        callback(&mut template);
        Action::from(LogData {
            name: entity_id.assumed_friendly_name().into(),
            entity_id: Some(entity_id),
            message: template.into(),
        })
    }
}

impl From<LogData> for Service {
    fn from(data: LogData) -> Self {
        Service::Log { data }
    }
}

#[derive(Serialize, Clone)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum Service {
    #[serde(rename = "input_number.set_value")]
    SetInputNumberValue {
        data: ServiceData<TemplatableValue<Value<usize>>>,
        target: ActionTarget,
    },
    #[serde(rename = "number.set_value")]
    SetNumberValue {
        data: ServiceData<TemplatableValue<Value<usize>>>,
        target: ActionTarget,
    },
    #[serde(rename = "logbook.log")]
    Log { data: LogData },
}

impl From<Service> for Action {
    fn from(value: Service) -> Self {
        Action::Service(value)
    }
}

impl From<LogData> for Action {
    fn from(data: LogData) -> Self {
        Action::Service(Service::Log { data })
    }
}

#[derive(Serialize, Clone)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum Action {
    #[serde(untagged)]
    Service(Service),
    #[serde(untagged)]
    Conditional(ConditionalActions),
    #[serde(untagged)]
    Stop(Stop),
}

impl Action {
    #[allow(unused)]
    pub fn comment(comment: impl Into<Rc<str>>) -> Action {
        Action::Stop(Stop {
            enabled: false,
            stop: comment.into(),
        })
    }
}

#[derive(Serialize, Clone)]
pub struct Stop {
    pub stop: Rc<str>,
    pub enabled: bool,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AutomationIdentifier {
    #[allow(unused)]
    Alias(String),
    #[allow(unused)]
    Id(String),
    #[serde(untagged)]
    Both { alias: String, id: String },
}

impl AutomationIdentifier {
    pub fn entity_id(&self) -> Option<EntityId> {
        match self {
            Self::Alias(_) => None,
            Self::Id(id) => Some(id.as_str()),
            Self::Both { id, .. } => Some(id.as_str()),
        }
        .map(|x| EntityId::external(EntityType::Automation, x))
    }
}

impl Package {
    pub fn new_automation_identifier(&self, name: &str) -> AutomationIdentifier {
        AutomationIdentifier::Both {
            alias: name.to_string(),
            id: self.new_identity_name_for::<Automation>(name).to_string(),
        }
    }
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct TraceOptions {
    pub stored_traces: u8,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct Automation {
    #[serde(flatten)]
    pub name: AutomationIdentifier,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trace: Option<TraceOptions>,
    pub trigger: Vec<TriggerHolder>,
    pub condition: Vec<Condition>,
    pub actions: Vec<Action>,
}

impl HasEntityType for Automation {
    fn entity_type() -> EntityType {
        EntityType::Automation
    }
}
