use core::ops::Mul;
use std::{borrow::Cow, collections::HashMap};

use serde::{Deserialize, Serialize};

use crate::{entity_id::{EntityId, EntityMember, EntityType, HasEntityType}, template::{IntoJinja as _, TemplateExpression}, types::{ComparableNumber, HasSensors, InsertableIn}};
#[derive(Serialize)]
#[serde(untagged)]
pub enum MaybeList<T> {
    Single(T),
    Multiple(Vec<T>),
}

impl<T: Clone> From<&[T]> for MaybeList<T> {
    fn from(value: &[T]) -> Self {
        MaybeList::Multiple(value.into())
    }
}

impl<T> From<Vec<T>> for MaybeList<T> {
    fn from(mut value: Vec<T>) -> Self {
        if let [_] = &value[..] {
            MaybeList::Single(value.pop().unwrap())
        } else {
            MaybeList::Multiple(value)
        }
    }
}

impl<T> From<T> for MaybeList<T> {
    fn from(value: T) -> Self {
        MaybeList::Single(value)
    }
}

#[derive(Serialize, Deserialize, Default, Clone, Hash, PartialEq, Eq)]
pub struct Duration {
    #[serde(default)]
    pub hours: usize,
    #[serde(default)]
    pub minutes: usize,
    #[serde(default)]
    pub seconds: usize,
}

impl Duration {
    pub fn to_seconds(&self) -> usize {
        (((self.hours * 60) + self.minutes) * 60) + self.seconds
    }
    pub fn from_seconds(total_seconds: usize) -> Self {
        let seconds = total_seconds % 60;
        let total_minutes = total_seconds / 60;
        let minutes = total_minutes % 60;
        let hours = total_minutes / 60;
        Duration {
            hours,
            minutes,
            seconds,
        }
    }
}

impl Mul<usize> for &Duration {
    type Output = Duration;

    fn mul(self, rhs: usize) -> Self::Output {
        Duration::from_seconds(self.to_seconds() * rhs)
    }
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct DerivativeSensor<'a> {
    pub name: String,
    pub source: EntityId<'a>,
    pub time_window: Duration,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unit_time: Option<UnitTime>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub round: Option<usize>,
}

impl HasEntityType for DerivativeSensor<'_> {
    fn entity_type() -> EntityType {
        EntityType::Sensor
    }
}

pub type Template = String;

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum UnitOfMeasurement {
    #[serde(rename = "°C")]
    Celcius,
    #[serde(rename = "%")]
    Percent,
    Minutes,
}

#[derive(Serialize)]
pub enum UnitTime {
    #[allow(unused)]
    #[serde(rename = "d")]
    Days,
    #[allow(unused)]
    #[serde(rename = "h")]
    Hours,
    #[allow(unused)]
    #[serde(rename = "min")]
    Minutes,
    #[allow(unused)]
    #[serde(rename = "s")]
    Seconds,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum DeviceClass {
    Temperature,
    PowerFactor,
    Duration,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum StateClass {
    Measurement,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct TemplateSensor {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub device_class: Option<DeviceClass>,
    pub state: Template,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state_class: Option<StateClass>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unit_of_measurement: Option<UnitOfMeasurement>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unique_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub availability: Option<String>,
}

impl TemplateSensor {
    pub fn set_availability_from(&mut self, entities: &[EntityMember]) {
        if entities.is_empty() {
            self.availability = None
        } else {
            self.availability = Some(TemplateExpression::fold(entities.iter().map(|x| x.to_ha_check()), |x, y| x.and(&y)).unwrap().to_jinja().into_owned())
        }
    }
}

impl HasEntityType for TemplateSensor {
    fn entity_type() -> EntityType {
        EntityType::Sensor
    }
}

#[derive(Serialize, Default)]
#[serde(rename_all = "snake_case")]
pub struct TemplateSensorHolder {
    pub sensor: Vec<TemplateSensor>,
}

#[derive(Serialize)]
#[serde(tag = "platform", rename_all = "snake_case")]
pub enum Sensor<'a> {
    Derivative(DerivativeSensor<'a>),
}

//#[derive(Serialize, Default)]
//#[serde(rename_all = "snake_case")]
//pub struct Derivatives<'a> {
//    #[serde(borrow)]
//    pub sensors: HashMap<Cow<'a, str>, DerivativeSensor<'a>>,
//}

impl HasSensors for Option<[TemplateSensorHolder; 1]> {
    fn is_empty(&self) -> bool {
        let Some(templates) = self else { return true };
        templates.is_empty()
    }
}

#[derive(Serialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum InputNumberMode {
    Box,
    #[default]
    Slider,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub struct InputNumber<'a> {
    pub name: Option<Cow<'a, str>>,
    pub unit_of_measurement: UnitOfMeasurement,
    pub min: ComparableNumber<2, i32>,
    pub max: ComparableNumber<2, i32>,
    pub step: ComparableNumber<2, i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon: Option<Cow<'a, str>>,
    pub mode: InputNumberMode,
}

impl HasEntityType for InputNumber<'_> {
    fn entity_type() -> EntityType {
        EntityType::InputNumber
    }
}

impl<'data: 'helper, 'helper> InsertableIn<Helpers<'helper>> for TemplateSensor {
    type Key = ();
    fn insert_into(self, _name: Self::Key, target: &mut Helpers<'helper>) -> bool {
        target.template.get_or_insert_default()[0].sensor.push(self);
        true
    }
}

impl<'data: 'helper, 'helper> InsertableIn<Helpers<'helper>> for DerivativeSensor<'data> {
    type Key = ();
    fn insert_into(self, _name: Self::Key, target: &mut Helpers<'helper>) -> bool {
        target.sensor.push(Sensor::Derivative(self));
        true
    }
}

impl<'data: 'helper, 'helper> InsertableIn<Helpers<'helper>> for InputNumber<'data> {
    type Key = EntityId<'data>;
    fn insert_into(self, name: Self::Key, target: &mut Helpers<'helper>) -> bool {
        debug_assert!(name.r#type == Self::entity_type());
        target.input_number.insert(name.id, self).is_none()
    }
}

#[derive(Serialize, Default)]
#[serde(rename_all = "snake_case")]
pub struct Helpers<'a> {
    #[serde(skip_serializing_if = "HasSensors::is_empty", borrow)]
    pub sensor: Vec<Sensor<'a>>,
    #[serde(skip_serializing_if = "HasSensors::is_empty")]
    pub template: Option<[TemplateSensorHolder; 1]>,
    #[serde(skip_serializing_if = "HasSensors::is_empty", borrow)]
    pub input_number: HashMap<Cow<'a, str>, InputNumber<'a>>,
}

impl<'a> Helpers<'a> {
    pub fn insert<'data: 'a, T: InsertableIn<Self>>(
        &mut self,
        key: T::Key,
        value: T,
    ) -> bool {
        value.insert_into(key, self)
    }
}
