use core::{f32, fmt};
use std::{
    any::TypeId, collections::HashMap, fmt::Display, hash::{Hash, Hasher}
};

use serde::Serialize;

use crate::entity_id::{EntityType, HasEntityType};

pub fn to_string_serialize<T: std::fmt::Display, S>(
    what: T,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    serializer.serialize_str(&what.to_string())
}

pub trait Number: fmt::Display {}
impl Number for i32 {}
impl Number for f32 {}
impl Number for usize {}
impl Number for u32 {}
impl<const N: usize, T: Number> Number for ComparableNumber<N, T> {}

pub enum ComparableNumber<const PRECISION: usize, T> {
    Float(f32),
    Int(T),
}

impl<const N: usize, T: 'static> From<T> for ComparableNumber<N, T> {
    fn from(value: T) -> Self {
        debug_assert!(TypeId::of::<T>() != TypeId::of::<f32>());
        Self::Int(value)
    }
}

impl<const N: usize, T: Clone> Clone for ComparableNumber<N, T> {
    fn clone(&self) -> Self {
        match self {
            Self::Float(value) => Self::Float(*value),
            Self::Int(value) => Self::Int(value.clone()),
        }
    }
}

impl<const N: usize, T: Copy> Copy for ComparableNumber<N, T> {}

impl<const N: usize, T> ComparableNumber<N, T> {
    fn to_string(value: f32) -> String {
        format!("{:.*}", N, value)
    }
}

impl<const N: usize, T: Display> Display for ComparableNumber<N, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match (self, f.precision()) {
            (Self::Float(value), None) => f.write_fmt(format_args!("{:.*}", N, value)),
            (Self::Float(value), Some(precision)) => {
                f.write_fmt(format_args!("{:.*}", precision, value))
            }
            (Self::Int(value), _) => value.fmt(f),
        }
    }
}

impl<const N: usize, T: Serialize> Serialize for ComparableNumber<N, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            ComparableNumber::Float(f) => serializer.serialize_f32(*f),
            ComparableNumber::Int(value) => value.serialize(serializer),
        }
    }
}

impl<const N: usize, T: Hash> Hash for ComparableNumber<N, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Int(value) => {
                state.write(b"Int(");
                value.hash(state);
                state.write(b")");
            }
            Self::Float(value) => {
                state.write(format!("Float({})", Self::to_string(*value)).as_bytes());
            }
        }
    }
}

impl<const N: usize, T: PartialEq> PartialEq for ComparableNumber<N, T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(value), Self::Int(other)) => value.eq(other),
            (Self::Float(value), Self::Float(other)) => {
                Self::to_string(*value) == Self::to_string(*other)
            }
            _ => false,
        }
    }
}

impl<const N: usize, T: Eq> Eq for ComparableNumber<N, T> {}

pub trait InsertableIn<T> {
    type Key;
    fn insert_into(self, name: Self::Key, target: &mut T) -> bool;
}

pub trait HasSensors {
    fn is_empty(&self) -> bool;
}
impl<T> HasSensors for Vec<T> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}

pub trait CustomizationFor {
    type Target: HasEntityType;
}

impl<T: CustomizationFor> HasEntityType for T {
    fn entity_type() -> EntityType {
        T::Target::entity_type()
    }
}

impl<K, V> HasSensors for HashMap<K, V> {
    fn is_empty(&self) -> bool {
        self.is_empty()
    }
}
