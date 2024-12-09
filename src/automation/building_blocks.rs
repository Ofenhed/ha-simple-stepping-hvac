use serde::Serialize;

use super::{Action, Condition};

#[derive(Serialize)]
pub struct Choose<'a> {
    pub conditions: Vec<Condition<'a>>,
    pub sequence: Vec<Action<'a>>,
}


#[derive(Serialize)]
#[serde(tag = "condition", content = "conditions")]
pub enum ConditionalActions<'a> {
    #[allow(unused)]
    If {
        #[serde(borrow)]
        r#if: Condition<'a>,
        r#then: Box<Action<'a>>,
    },
    #[allow(unused)]
    #[serde(untagged)]
    Choose {
        choose: Vec<Choose<'a>>,
    },
}

impl<'a> From<ConditionalActions<'a>> for Action<'a> {
    fn from(value: ConditionalActions<'a>) -> Self {
        Action::Conditional(value)
    }
}

impl<'a> From<Vec<Choose<'a>>> for Action<'a> {
    fn from(value: Vec<Choose<'a>>) -> Self {
        ConditionalActions::Choose {
            choose: value
        }.into()
    }
}
