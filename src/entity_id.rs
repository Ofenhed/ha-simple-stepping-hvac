use std::{borrow::Cow, marker::PhantomData};

use serde::{de::{self, Visitor}, Deserialize, Serialize};

use crate::{template::TemplateExpression, types::to_string_serialize, Package};

#[derive(strum::EnumString, strum::IntoStaticStr, strum::Display, Clone, Hash, PartialEq, Eq, Copy)]
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
pub struct EntityId<'a> {
    pub r#type: EntityType,
    pub id: Cow<'a, str>,
}

impl<'a> EntityId<'a> {
    pub fn external(r#type: EntityType, id: Cow<'a, str>) -> Self {
        Self {
            r#type,
            id: EntityId::encode_string(&id),
        }
    }

    pub fn assumed_friendly_name(&self) -> String {
        let mut capital = true;
        self.id.chars().map(|x|
            if x == '_' {
                capital = true;
                ' '
            } else if capital {
                capital = false;
                x.to_ascii_uppercase()
            } else {
                x
            }).collect()
    }
}

impl<'a: 'b, 'b> EntityId<'a> {
    pub fn borrow(&'b self) -> EntityId<'b> {
        EntityId::external(self.r#type.clone(), match &self.id {
            Cow::Borrowed(borrowed) => Cow::Borrowed(borrowed),
            Cow::Owned(owned) => Cow::Borrowed(&owned),
        })
    }
}

impl EntityId<'static> {
    pub fn encode_string(name: &str) -> Cow<'static, str> {
        let mut encoded = name.to_lowercase().replace(' ', "_").replace('.', "_");
        encoded.retain(|c| (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || (c == '_'));
        encoded.into()
    }
}


impl EntityId<'_> {
    pub fn into_static(self) -> EntityId<'static> {
        EntityId::external(self.r#type.clone(), match self.id {
            Cow::Borrowed(borrowed) => Cow::Owned(borrowed.to_owned()),
            Cow::Owned(owned) => Cow::Owned(owned),
        })
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum EntityMemberType<'a> {
    State,
    Attribute(Cow<'a, str>),
}

impl<'a> From<&'a str> for EntityMemberType<'a> {
    fn from(value: &'a str) -> Self {
        Self::Attribute(Cow::Borrowed(value))
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub struct EntityMember<'a>(pub EntityId<'a>, pub EntityMemberType<'a>);

impl<'a> HasEntityId for EntityMember<'a> {
    fn entity_id(&self) -> EntityId<'a> {
        self.0.clone()
    }
}

impl<'a> EntityMember<'a> {
    pub fn to_ha_call(&self) -> TemplateExpression<'static> {
        match &self.1 {
            EntityMemberType::State => TemplateExpression(format!("states('{}',rounded=False)", self.0).into()),
            EntityMemberType::Attribute(cow) => TemplateExpression(format!("state_attr('{}', '{cow}')", self.0).into()),
        }
    }

    pub fn to_ha_check(&self) -> TemplateExpression<'static> {
        match &self.1 {
            EntityMemberType::State => TemplateExpression(format!("has_value('{}')", self.0).into()),
            EntityMemberType::Attribute(cow) => TemplateExpression(format!("(has_value('{entity}') and state_attr('{entity}', '{cow}') is not none)", entity = self.0).into()),
        }
    }

    pub fn state(entity: EntityId<'a>) -> Self {
        EntityMember(entity, EntityMemberType::State)
    }

    pub fn state_entity(&self) -> Option<EntityId<'a>> {
        match &self.1 {
            EntityMemberType::State => Some(self.0.clone()),
            EntityMemberType::Attribute(_cow) => None,
        }
    }

    pub fn static_state_entity(&self) -> Option<EntityId<'static>> {
        match &self.1 {
            EntityMemberType::State => Some(self.0.clone().into_static()),
            EntityMemberType::Attribute(_cow) => None,
        }
    }
}

impl EntityMember<'_> {
    pub fn into_static(self) -> EntityMember<'static> {
        EntityMember(self.0.into_static(), match self.1 {
            EntityMemberType::State => EntityMemberType::State,
            EntityMemberType::Attribute(Cow::Borrowed(borrowed)) => EntityMemberType::Attribute(Cow::Owned(borrowed.to_string())),
            EntityMemberType::Attribute(Cow::Owned(owned)) => EntityMemberType::Attribute(Cow::Owned(owned)),
        })
    }
}

impl<'a: 'b, 'b> EntityMember<'a> {
    #[allow(unused)]
    pub fn entity_id(&'b self) -> EntityId<'b> {
        match &self.0 {
            EntityId { r#type, id: Cow::Owned(value) } => EntityId::external(r#type.clone(), Cow::Borrowed(value)),
            borrowed@EntityId { id: Cow::Borrowed(_), .. } => borrowed.clone(),
        }
    }
    #[allow(unused)]
    pub fn attribute(&'b self) -> Option<Cow<'b, str>> {
        match &self.1 {
            EntityMemberType::State => None,
            EntityMemberType::Attribute(cow) => Some(cow.clone()),
        }
    }
}

impl EntityMember<'static> {
    pub fn static_entity_id(&self) -> EntityId<'static> {
        self.0.clone()
    }
    pub fn static_attribute(&self) -> Option<Cow<'static, str>> {
        match &self.1 {
            EntityMemberType::State => None,
            EntityMemberType::Attribute(cow) => Some(cow.clone()),
        }
    }
}

#[derive(Default)]
struct EntityDeserializer<'a> {
    phantom: PhantomData<&'a ()>,
}
impl<'a, 'de: 'a> Visitor<'de> for EntityDeserializer<'a> {
    type Value = EntityId<'a>;

    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(formatter, "An entity id, based on a domain and a name")
    }

    fn visit_borrowed_str<E>(self, v: &'de str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        match v.find(|x| x == '.') {
            Some(dot_at) => match v[..dot_at].try_into() {
                Ok(entity_type) => Ok(EntityId::external(entity_type, Cow::Borrowed(&v[dot_at+1..]))),
                Err(_) => Err(de::Error::invalid_type(de::Unexpected::Str(v), &self)),
            },
            _ => Err(de::Error::invalid_type(de::Unexpected::Str(v), &self)),
        }
    }

    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> where E: de::Error, {
        let EntityId { r#type, id } = self.visit_borrowed_str(v)?;
        Ok(EntityId::external(r#type, Cow::Owned(id.to_string())))
    }

    fn visit_string<E>(self, v: String) -> Result<Self::Value, E> where E: de::Error, {
        let EntityId { r#type, id } = self.visit_borrowed_str(&v)?;
        Ok(EntityId::external(r#type, Cow::Owned(id.to_string())))
    }
}

impl<'de: 'a, 'a> Deserialize<'de> for EntityId<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_string(EntityDeserializer::default())
    }
}

impl std::fmt::Display for EntityId<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}.{}", self.r#type, self.id))
    }
}

impl Serialize for EntityId<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string_serialize(self, serializer)
    }
}

impl std::fmt::Display for EntityMember<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let after = match &self.1 {
            EntityMemberType::State => Cow::Borrowed("state"),
            EntityMemberType::Attribute(cow) => Cow::Owned(format!("attributes.{}", cow)),
        };
        f.write_fmt(format_args!("{}.{}", self.0, after))
    }
}

impl Serialize for EntityMember<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string_serialize(self, serializer)
    }
}

pub trait HasEntityId {
    #[allow(unused)]
    fn entity_id(&self) -> EntityId<'_>;
}

pub trait HasEntityType {
    fn entity_type() -> EntityType;
}

impl EntityType {
    pub fn of<T: HasEntityType>(_: &T) -> EntityType {
        T::entity_type()
    }
}

impl Package<'_> {
    pub fn new_entity_id(&self, entity_type: EntityType, name: &str) -> EntityId<'static> {
        let mut counter = 0;
        let mut known_entity_ids = self.known_entity_ids.lock().expect("We will not mess up this lock");
        loop {
            let ctr: Cow<str> = if counter > 0 {
                counter.to_string().into()
            } else {
                "".into()
            };
            let entity_id = EntityId::external(entity_type, format!("{}_{name}{ctr}", self.entity_id_prefix).into());
            if known_entity_ids.insert(entity_id.clone().into_static()) {
                return entity_id
            }
            counter += 1;
        }
    }

    pub fn new_identity_name_for<T: HasEntityType>(&self, name: &str) -> Cow<'static, str> {
        self.new_entity_id(T::entity_type(), name).id
    }
}
