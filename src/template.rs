use std::{borrow::{Borrow, Cow}, ops::{Add, Div, Mul, Rem, Sub}};
use crate::{automation::Condition, entity_id::EntityMember, types::Number};

pub struct TemplateExpression<'a>(pub Cow<'a, str>);

impl TemplateExpression<'_> {
    pub fn to_condition(&self) -> Condition<'static> {
        Condition::Template { value_template: self.to_jinja() }
    }
}

impl From<&TemplateExpression<'_>> for Condition<'static> {
    fn from(value: &TemplateExpression<'_>) -> Self {
        value.to_condition()
    }
}

impl From<&EntityMember<'_>> for TemplateExpression<'static> {
    fn from(value: &EntityMember<'_>) -> Self {
        value.to_ha_call()
    }
}

impl<T: Number> Sub<T> for TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn sub(self, rhs: T) -> Self::Output {
        TemplateExpression(format!("({} - {})", self.0, rhs).into())
    }
}

impl<T: Number> Add<T> for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn add(self, rhs: T) -> Self::Output {
        TemplateExpression(format!("({} + {})", self.0, rhs).into())
    }
}

impl<T: Number> Mul<T> for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn mul(self, rhs: T) -> Self::Output {
        TemplateExpression(format!("({} * {})", self.0, rhs).into())
    }
}

impl<T: Number> Div<T> for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn div(self, rhs: T) -> Self::Output {
        TemplateExpression(format!("({} / {})", self.0, rhs).into())
    }
}

impl<T: Number> Rem<T> for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn rem(self, rhs: T) -> Self::Output {
        TemplateExpression(format!("({} % {})", self.0, rhs).into())
    }
}

impl Sub for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn sub(self, rhs: Self) -> Self::Output {
        TemplateExpression(format!("({} - {})", self.0, rhs.0).into())
    }
}

impl Add for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn add(self, rhs: Self) -> Self::Output {
        TemplateExpression(format!("({} + {})", self.0, rhs.0).into())
    }
}

impl Mul for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn mul(self, rhs: Self) -> Self::Output {
        TemplateExpression(format!("({} * {})", self.0, rhs.0).into())
    }
}

impl Div for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn div(self, rhs: Self) -> Self::Output {
        TemplateExpression(format!("({} / {})", self.0, rhs.0).into())
    }
}

impl Rem for &TemplateExpression<'_> {
    type Output = TemplateExpression<'static>;

    fn rem(self, rhs: Self) -> Self::Output {
        TemplateExpression(format!("({} % {})", self.0, rhs.0).into())
    }
}

impl TemplateExpression<'_> {
    pub fn gt(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} > {})", self.0, rhs.0).into())
    }
    pub fn ge(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} >= {})", self.0, rhs.0).into())
    }
    pub fn lt(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} < {})", self.0, rhs.0).into())
    }
    pub fn le(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} <= {})", self.0, rhs.0).into())
    }
    pub fn eq(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} == {})", self.0, rhs.0).into())
    }
    pub fn and(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} and {})", self.0, rhs.0).into())
    }
    pub fn or(&self, rhs: &TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} or {})", self.0, rhs.0).into())
    }
    pub fn constant<T: Number>(constant: T) -> TemplateExpression<'static> {
        TemplateExpression(format!("{}", constant).into())
    }
    pub fn to_float(&self) -> TemplateExpression<'static> {
        TemplateExpression(format!("float({})", self.0).into())
    }
    pub fn to_int(&self) -> TemplateExpression<'static> {
        TemplateExpression(format!("int({})", self.0).into())
    }
    pub fn round(&self, precision: usize) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} | round({}))", self.0, precision).into())
    }
    pub fn as_raw_template<'a>(&'a self) -> Cow<'a, str> {
        Cow::Borrowed(self.0.as_ref())
    }
    pub fn abs(&self) -> TemplateExpression<'static> {
        TemplateExpression(format!("({} | abs)", self.0).into())
    }
    pub fn now() -> TemplateExpression<'static> {
        TemplateExpression(Cow::Borrowed("now()"))
    }
    pub fn this_automation_last_trigger() -> TemplateExpression<'static> {
        TemplateExpression(Cow::Borrowed("this.attributes.last_triggered"))
    }
    pub fn timedelta(hours: TemplateExpression<'_>, minutes: TemplateExpression<'_>, seconds: TemplateExpression<'_>) -> TemplateExpression<'static> {
        TemplateExpression(format!("timedelta(hours={hours}, minutes={minutes}, seconds={seconds})", hours=hours.as_raw_template(), minutes=minutes.as_raw_template(), seconds=seconds.as_raw_template()).into())
    }
}
impl<'s> TemplateExpression<'s> {
    pub fn max<S: Borrow<Self>>(values: impl Iterator<Item = S>) -> Option<TemplateExpression<'static>> {
        let Some(inner) = TemplateExpression::fold(values, |other, new| TemplateExpression(format!("{}, {}", other.as_raw_template(), new.as_raw_template()).into())) else { return None };
        Some(TemplateExpression(format!("max({})", inner.as_raw_template()).into()))
    }

    pub fn average<S: Borrow<Self>>(values: impl Iterator<Item = S>) -> Option<TemplateExpression<'static>> {
        let mut count = 0;
        let sum = TemplateExpression::fold(values.map(|x| {
            count += 1;
                x
        }), |other, new| &other + &new);
        sum.map(|sum|
        TemplateExpression(format!("{sum} / {count}", sum = sum.as_raw_template()).into()))
    }

    pub fn fold<S: Borrow<Self>>(mut iter: impl Iterator<Item = S>, join: impl Fn(Self, Self) -> TemplateExpression<'static>) -> Option<TemplateExpression<'static>> {
        let mut result = {
            let Some(first) = iter.next() else { return None };
            first.borrow().0.clone()
        };
        while let Some(next) = iter.next() {
            result = join(TemplateExpression(result), TemplateExpression(next.borrow().0.clone())).0;
        }
        Some(TemplateExpression(match result {
            Cow::Borrowed(borrowed) => borrowed.to_owned().into(),
            Cow::Owned(owned) => owned.into(),
        }))
    }
}

pub struct JinjaTemplate<'a>(pub Cow<'a, str>);

pub trait IntoJinja {
    fn to_jinja(self) -> Cow<'static, str>;
}

impl IntoJinja for &TemplateExpression<'_> {
    fn to_jinja(self) -> Cow<'static, str> {
        format!("{{{{ {} }}}}", self.0).into()
    }
}

impl IntoJinja for TemplateExpression<'_> {
    fn to_jinja(self) -> Cow<'static, str> {
        (&self).to_jinja()
    }
}

impl IntoJinja for JinjaTemplate<'_> {
    fn to_jinja(self) -> Cow<'static, str> {
        Cow::Owned(self.0.into_owned())
    }
}
