pub mod building_blocks;

use std::{borrow::Cow, fmt::Display};

use building_blocks::ConditionalActions;
use serde::Serialize;

use crate::{entity_id::{EntityId, EntityMember, EntityType, HasEntityType}, helpers::Duration, template::TemplateExpression, types::{to_string_serialize, ComparableNumber}, Package};

#[derive(Default)]
pub enum TimeInterval {
    At(u8),
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
            Self::Unset => Ok(())
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "trigger", rename_all = "snake_case")]
pub enum Trigger<'a> {
    State { entity_id: EntityId<'a> },
    TimePattern {
        #[serde(serialize_with = "to_string_serialize", skip_serializing_if = "TimeInterval::is_none")]
        hours: TimeInterval,
        #[serde(serialize_with = "to_string_serialize", skip_serializing_if = "TimeInterval::is_none")]
        minutes: TimeInterval,
        #[serde(serialize_with = "to_string_serialize", skip_serializing_if = "TimeInterval::is_none")]
        seconds: TimeInterval,
    },
}

#[derive(Serialize)]
pub struct TriggerHolder<'a> {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<Cow<'a, str>>,
    #[serde(flatten)]
    pub trigger: Trigger<'a>,
}

impl<'a> From<Trigger<'a>> for TriggerHolder<'a> {
    fn from(value: Trigger<'a>) -> Self {
        Self {
            id: None,
            trigger: value,
        }
    }
}

#[derive(Serialize, Clone, Hash, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum NumericStateComparison<'a> {
    #[allow(unused)]
    Above(ComparableNumber<2, i32>),
    #[allow(unused)]
    Below(ComparableNumber<2, i32>),
    #[allow(unused)]
    #[serde(rename = "above")]
    AboveEntityState(EntityId<'a>),
    #[allow(unused)]
    #[serde(rename = "below")]
    BelowEntityState(EntityId<'a>),
}

impl EntityMember<'static> {
    pub fn bigger_than(self, other: f32) -> Condition<'static> {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::Above(ComparableNumber::Float(other)),
        }
    }
    pub fn smaller_than(self, other: f32) -> Condition<'static> {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::Below(ComparableNumber::Float(other)),
        }
    }
    pub fn bigger_than_entity<'a>(self, other: EntityId<'a>) -> Condition<'a> {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::AboveEntityState(other),
        }
    }
    pub fn smaller_than_entity<'a>(self, other: EntityId<'a>) -> Condition<'a> {
        Condition::NumericState {
            entity_id: self.static_entity_id(),
            attribute: self.static_attribute(),
            compared: NumericStateComparison::BelowEntityState(other),
        }
    }
}

#[derive(Serialize, Clone, Hash, PartialEq, Eq)]
#[serde(tag = "condition", rename_all = "snake_case")]
pub enum Condition<'a> {
    #[allow(unused)]
    State {
        entity_id: EntityId<'a>,
        #[serde(skip_serializing_if = "Option::is_none")]
        attribute: Option<Cow<'a, str>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        r#for: Option<Duration>,
        #[serde(skip_serializing_if = "Option::is_none")]
        state: Option<Cow<'a, str>>,
    },
    #[allow(unused)]
    NumericState {
        entity_id: EntityId<'a>,
        #[serde(skip_serializing_if = "Option::is_none")]
        attribute: Option<Cow<'a, str>>,
        #[serde(flatten)]
        compared: NumericStateComparison<'a>,
    },
    Trigger {
        id: Cow<'a, str>,
        enabled: bool,
    },
    Template {
        value_template: Cow<'a, str>,
    },
    #[allow(unused)]
    Or {
        conditions: Vec<Condition<'a>>,
    },
    #[allow(unused)]
    And {
        conditions: Vec<Condition<'a>>,
    },
    #[allow(unused)]
    Not {
        conditions: Vec<Condition<'a>>,
    },
}

impl<'data: 'cond, 'cond> Condition<'cond> {
    pub fn comment(text: &'data str) -> Self {
        Self::Trigger {
            id: Cow::Borrowed(text),
            enabled: false,
        }
    }
}
impl Condition<'_> {
    pub fn not(self) -> Self {
        Self::Not {
            conditions: vec![self],
        }
    }

    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (Self::And { mut conditions }, Self::And { conditions: mut other }) => {
                conditions.append(&mut other);
                Self::And { conditions }
            }
            (Self::And { mut conditions }, other) => {
                conditions.push(other);
                Self::And { conditions }
            }
            (other, Self::And { mut conditions })  => {
                let mut new = Vec::with_capacity(conditions.len() + 1);
                new.push(other);
                new.append(&mut conditions);
                Self::And { conditions: new }
            }
            (first, second) => Self::And { conditions: vec![first, second] }
        }
    }

    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Self::Or { mut conditions }, Self::Or { conditions: mut other }) => {
                conditions.append(&mut other);
                Self::Or { conditions }
            }
            (Self::Or { mut conditions }, other) => {
                conditions.push(other);
                Self::Or { conditions }
            }
            (other, Self::Or { mut conditions })  => {
                let mut new = Vec::with_capacity(conditions.len() + 1);
                new.push(other);
                new.append(&mut conditions);
                Self::Or { conditions: new }
            }
            (first, second) => Self::Or { conditions: vec![first, second] }
        }
    }
}

#[derive(Serialize)]
pub struct Value<T> {
    value: T,
}

impl<T> From<T> for Value<T> {
    fn from(value: T) -> Self {
        Self { value }
    }
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum TemplatableValue<'a, T> {
    #[allow(unused)]
    Value(T),
    #[allow(unused)]
    Template(Cow<'a, str>),
}

impl<T> From<TemplateExpression<'_>> for TemplatableValue<'static, T> {
    fn from(value: TemplateExpression<'_>) -> Self {
        Self::Template(format!("{{{{ {} }}}}", value.0).into())
    }
}

impl<T> From<TemplateExpression<'_>> for ServiceData<TemplatableValue<'static, T>> {
    fn from(value: TemplateExpression<'_>) -> Self {
        Self {
            value: TemplatableValue::Template(format!("{{{{ {} }}}}", value.0).into())
        }
    }
}

#[derive(Serialize)]
pub struct ActionTarget<'a> {
    pub entity_id: EntityId<'a>,
}

impl<'a> From<EntityId<'a>> for ActionTarget<'a> {
    fn from(value: EntityId<'a>) -> Self {
        Self { entity_id: value }
    }
}

#[derive(Serialize)]
pub struct ServiceData<T> {
    pub value: T,
}

impl<T> From<T> for ServiceData<T> {
    fn from(value: T) -> Self {
        ServiceData {
            value
        }
    }
}

#[derive(Serialize)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum Service<'a> {
    #[serde(rename = "input_number.set_value")]
    SetInputNumberValue {
        data: ServiceData<TemplatableValue<'a, Value<usize>>>,
        target: ActionTarget<'a>,
    },
    #[serde(rename = "number.set_value")]
    SetNumberValue {
        data: ServiceData<TemplatableValue<'a, Value<usize>>>,
        target: ActionTarget<'a>,
    },
}

impl<'a> From<Service<'a>> for Action<'a> {
    fn from(value: Service<'a>) -> Self {
        Action::Service(value)
    }
}

#[derive(Serialize)]
#[serde(tag = "action", rename_all = "snake_case")]
pub enum Action<'a> {
    #[serde(untagged)]
    Service(Service<'a>),
    #[serde(untagged)]
    Conditional(ConditionalActions<'a>),
    #[serde(untagged)]
    Stop(Stop<'a>),
}

impl<'a> Action<'a> {
    #[allow(unused)]
    pub fn comment(comment: &'a str) -> Action<'a> {
        Action::Stop(Stop{
            enabled: false,
            stop: Cow::Borrowed(comment),
        })
    }
}

#[derive(Serialize)]
pub struct Stop<'a> {
    pub stop: Cow<'a, str>,
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
    Both {
        alias: String,
        id: String,
    }
}

impl Package<'_> {
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
pub struct Automation<'a> {
    #[serde(flatten)]
    pub name: AutomationIdentifier,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trace: Option<TraceOptions>,
    pub trigger: Vec<TriggerHolder<'a>>,
    pub condition: Vec<Condition<'a>>,
    pub actions: Vec<Action<'a>>,
}

impl HasEntityType for Automation<'_> {
    fn entity_type() -> EntityType {
        EntityType::Automation

    }
}
