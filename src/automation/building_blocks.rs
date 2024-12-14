use serde::Serialize;

use super::{Action, Condition};

#[derive(Serialize, Clone)]
pub struct Choose {
    pub conditions: Vec<Condition>,
    pub sequence: Vec<Action>,
}

#[derive(Serialize, Clone)]
#[serde(tag = "condition", content = "conditions")]
pub enum ConditionalActions {
    #[allow(unused)]
    If {
        r#if: Condition,
        r#then: Box<Action>,
    },
    #[allow(unused)]
    #[serde(untagged)]
    Choose { choose: Vec<Choose> },
}

impl From<ConditionalActions> for Action {
    fn from(value: ConditionalActions) -> Self {
        Action::Conditional(value)
    }
}

impl From<Vec<Choose>> for Action {
    fn from(value: Vec<Choose>) -> Self {
        ConditionalActions::Choose { choose: value }.into()
    }
}
