use std::{borrow::Cow, rc::Rc};

use serde::{
    de::{self, Visitor},
    Deserialize, Serialize,
};

use crate::{template::TemplateExpression, types::to_string_serialize, Package};

#[derive(
    strum::EnumString, strum::IntoStaticStr, strum::Display, Clone, Hash, PartialEq, Eq, Copy,
)]
pub enum EntityType {
    #[strum(serialize = "climate")]
    Climate,
    #[strum(serialize = "sensor")]
    Sensor,
    #[strum(serialize = "number")]
    Number,
    #[strum(serialize = "automation")]
    Automation,
    #[strum(serialize = "input_number")]
    InputNumber,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct EntityId {
    pub r#type: EntityType,
    pub id: Rc<str>,
}

impl EntityId {
    pub fn attribute(self, attr: impl Into<Rc<str>>) -> EntityMember {
        EntityMember(self, EntityMemberType::from(attr))
    }

    pub fn state(self) -> EntityMember {
        EntityMember(self, EntityMemberType::State)
    }
}

impl EntityId {
    pub fn external(r#type: EntityType, id: &str) -> Self {
        Self {
            r#type,
            id: EntityId::encode_string(id).into(),
        }
    }

    pub fn to_ha_call_pretty(&self) -> Rc<TemplateExpression> {
        TemplateExpression::fun(
            "states",
            [
                (None, TemplateExpression::string(self.to_string())),
                (
                    Some(Cow::Borrowed("rounded")),
                    TemplateExpression::bool(true),
                ),
                (
                    Some(Cow::Borrowed("with_unit")),
                    TemplateExpression::bool(true),
                ),
            ],
        )
        .mark_const_expr()
    }
    pub fn assumed_friendly_name(&self) -> String {
        let mut capital = true;
        self.id
            .chars()
            .map(|x| {
                if x == '_' {
                    capital = true;
                    ' '
                } else if capital {
                    capital = false;
                    x.to_ascii_uppercase()
                } else {
                    x
                }
            })
            .collect()
    }
}

impl EntityId {
    pub fn encode_string(name: &str) -> String {
        let mut encoded = name.to_lowercase().replace(' ', "_").replace('.', "_");
        encoded.retain(|c| (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || (c == '_'));
        encoded.into()
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum EntityMemberType {
    State,
    Attribute(Rc<str>),
}

impl<I: Into<Rc<str>>> From<I> for EntityMemberType {
    fn from(value: I) -> Self {
        Self::Attribute(value.into())
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct EntityMember(pub EntityId, pub EntityMemberType);

impl HasEntityId for EntityMember {
    fn entity_id(&self) -> EntityId {
        self.0.clone()
    }
}

impl EntityMember {
    pub fn to_ha_call_named(&self, name: impl Into<Rc<str>>) -> Rc<TemplateExpression> {
        self.to_ha_call().mark_named_const_expr(name)
    }
    pub fn to_ha_call(&self) -> Rc<TemplateExpression> {
        match &self.1 {
            EntityMemberType::State => TemplateExpression::fun(
                "states",
                [
                    (None, TemplateExpression::string(self.0.to_string())),
                    (
                        Some(Cow::Borrowed("rounded")),
                        TemplateExpression::bool(false),
                    ),
                ],
            )
            .mark_const_expr(),
            EntityMemberType::Attribute(cow) => TemplateExpression::fun(
                "state_attr",
                [
                    (None, TemplateExpression::string(self.0.to_string())),
                    (None, TemplateExpression::string(cow.clone())),
                ],
            )
            .mark_const_expr(),
        }
    }

    pub fn to_ha_check(&self) -> Rc<TemplateExpression> {
        let entity = TemplateExpression::string(self.0.to_string());
        match &self.1 {
            EntityMemberType::State => {
                TemplateExpression::fun("has_value", [(None, entity.clone())]).mark_const_expr()
            }
            EntityMemberType::Attribute(cow) => {
                TemplateExpression::fun("has_value", [(None, entity.clone())])
                    .and(
                        TemplateExpression::fun(
                            "state_attr",
                            [
                                (None, entity.clone()),
                                (None, TemplateExpression::string(cow.clone())),
                            ],
                        )
                        .is_not_none(),
                    )
                    .mark_const_expr()
            }
        }
    }

    pub fn state_entity(&self) -> Option<EntityId> {
        match &self.1 {
            EntityMemberType::State => Some(self.0.clone()),
            EntityMemberType::Attribute(_cow) => None,
        }
    }
}

impl EntityMember {
    #[allow(unused)]
    pub fn entity_id(&self) -> EntityId {
        self.0.clone()
    }
    #[allow(unused)]
    pub fn attribute(&self) -> Option<Rc<str>> {
        match &self.1 {
            EntityMemberType::State => None,
            EntityMemberType::Attribute(cow) => Some(cow.clone()),
        }
    }
}

impl EntityMember {
    pub fn static_entity_id(&self) -> EntityId {
        self.0.clone()
    }
    pub fn static_attribute(&self) -> Option<Rc<str>> {
        match &self.1 {
            EntityMemberType::State => None,
            EntityMemberType::Attribute(cow) => Some(cow.clone()),
        }
    }
}

#[derive(Default)]
struct EntityDeserializer {}
impl<'de> Visitor<'de> for EntityDeserializer {
    type Value = EntityId;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "An entity id, based on a domain and a name")
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match v.find(|x| x == '.') {
            Some(dot_at) => match v[..dot_at].try_into() {
                Ok(entity_type) => Ok(EntityId::external(entity_type, &v[dot_at + 1..])),
                Err(_) => Err(de::Error::invalid_type(de::Unexpected::Str(v), &self)),
            },
            _ => Err(de::Error::invalid_type(de::Unexpected::Str(v), &self)),
        }
    }
}

impl<'de> Deserialize<'de> for EntityId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(EntityDeserializer::default())
    }
}

impl std::fmt::Display for EntityId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.r#type, self.id))
    }
}

impl Serialize for EntityId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string_serialize(self, serializer)
    }
}

impl std::fmt::Display for EntityMember {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let after = match &self.1 {
            EntityMemberType::State => Cow::Borrowed("state"),
            EntityMemberType::Attribute(cow) => Cow::Owned(format!("attributes.{}", cow)),
        };
        f.write_fmt(format_args!("{}.{}", self.0, after))
    }
}

impl Serialize for EntityMember {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string_serialize(self, serializer)
    }
}

pub trait HasEntityId {
    #[allow(unused)]
    fn entity_id(&self) -> EntityId;
}

pub trait HasEntityType {
    fn entity_type() -> EntityType;
}

impl EntityType {
    pub fn of<T: HasEntityType>(_: &T) -> EntityType {
        T::entity_type()
    }
}

impl Package {
    pub fn new_entity_id(&self, entity_type: EntityType, name: &str) -> EntityId {
        let mut counter = 0;
        let mut known_entity_ids = self
            .state
            .known_entity_ids
            .lock()
            .expect("We will not mess up this lock");
        loop {
            let ctr: Cow<str> = if counter > 0 {
                counter.to_string().into()
            } else {
                "".into()
            };
            let entity_id = EntityId::external(
                entity_type,
                &format!("{}_{name}{ctr}", self.state.entity_id_prefix),
            );
            if known_entity_ids.insert(entity_id.clone()) {
                return entity_id;
            }
            counter += 1;
        }
    }

    pub fn new_identity_name_for<T: HasEntityType>(&self, name: &str) -> Rc<str> {
        self.new_entity_id(T::entity_type(), &name).id
    }
}
