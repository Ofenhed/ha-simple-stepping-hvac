use std::{
    any::TypeId,
    collections::HashMap,
    fmt::Display,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
    sync::RwLock,
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

#[derive(Debug)]
pub struct LinkedTree<T> {
    value: T,
    parent: RwLock<Weak<LinkedTree<T>>>,
    children: RwLock<Vec<Rc<LinkedTree<T>>>>,
}

impl<T> Deref for LinkedTree<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for LinkedTree<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T: Clone> LinkedTree<T> {
    pub fn cloned(&self) -> T {
        self.value.clone()
    }
}

impl<'t, T: std::fmt::Debug + 't> LinkedTree<T> {
    pub fn new(value: T) -> Rc<Self> {
        Self {
            value,
            parent: Default::default(),
            children: Default::default(),
        }
        .into()
    }
    pub fn new_child(self: &Rc<Self>, value: T) -> Rc<Self> {
        let child: Rc<Self> = Self {
            value,
            parent: Rc::downgrade(&self).into(),
            children: Default::default(),
        }
        .into();
        self.children.write().unwrap().push(child.clone());
        child
    }

    pub fn parent(self: &Rc<Self>) -> Option<Rc<Self>> {
        Weak::upgrade(&self.parent.read().unwrap())
    }

    pub fn take(self: &Rc<Self>) -> Option<Rc<Self>> {
        let ret = if let Some(parent) = self.parent() {
            parent.children.write().unwrap().retain(|x| {
                eprint!(".");
                !Rc::ptr_eq(&self, &x)
            });
            Some(self.clone())
        } else {
            None
        };
        *self.parent.write().unwrap() = Weak::default();
        ret
    }

    pub fn take_leaf(self: &Rc<Self>) -> Option<Rc<Self>> {
        if let Some(first_child) = self.children().next() {
            first_child.take_leaf()
        } else {
            self.take()
        }
    }

    #[allow(unused)]
    pub fn children(self: &Rc<Self>) -> impl Iterator<Item = Rc<Self>> {
        let children = self.children.read().unwrap().clone();

        children.into_iter()
    }

    pub fn has_children(self: &Rc<Self>) -> bool {
        !self.children.read().unwrap().is_empty()
    }

    #[allow(unused)]
    pub fn replace_parent(self: &Rc<Self>, new: Rc<Self>) {
        *new.parent.write().unwrap() = Rc::downgrade(&self).into();
        if let Some(parent) = Weak::upgrade(&self.parent.write().unwrap()) {
            let mut parents_children = parent.children.write().unwrap();
            for child in parents_children.iter_mut() {
                if Rc::ptr_eq(child, &self) {
                    *child = new.clone();
                    return;
                }
            }
        }
    }
}
