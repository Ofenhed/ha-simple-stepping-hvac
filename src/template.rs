use serde::Serialize;

use crate::types::to_string_serialize;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Write,
    ops::{Add, Deref, DerefMut, Div, Mul, Rem, Sub},
    rc::Rc,
    sync::Mutex,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TemplateOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Gt,
    Ge,
    Lt,
    Le,
    Eq,
    Ne,
    Or,
    And,
}

impl TemplateOp {
    fn fmt_raw_template(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => f.write_str("+"),
            Self::Sub => f.write_str("-"),
            Self::Mul => f.write_str("*"),
            Self::Div => f.write_str("/"),
            Self::Rem => f.write_str("%"),
            Self::Gt => f.write_str(">"),
            Self::Ge => f.write_str(">="),
            Self::Lt => f.write_str("<"),
            Self::Le => f.write_str("<="),
            Self::Eq => f.write_str("=="),
            Self::Ne => f.write_str("!="),
            Self::Or => f.write_str(" or "),
            Self::And => f.write_str(" and "),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TemplateUnaryOp {
    Not,
    IsNone,
    IsNotNone,
}

#[derive(Default, Clone)]
struct TemplateState {
    const_expressions: HashMap<Rc<TemplateExpression>, Rc<TemplateExpression>>,
    variables: HashSet<Rc<str>>,
}

struct Formatter<'a: 'b, 'b> {
    f: &'b mut std::fmt::Formatter<'a>,
    state: &'b mut TemplateState,
}

impl<'a> Deref for Formatter<'a, '_> {
    type Target = std::fmt::Formatter<'a>;

    fn deref(&self) -> &Self::Target {
        self.f
    }
}

impl<'a> DerefMut for Formatter<'a, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.f
    }
}

impl TemplateControl {}

impl TemplateUnaryOp {
    fn fmt_raw_template(&self, f: &mut Formatter, expr: &TemplateExpression) -> std::fmt::Result {
        match self {
            Self::Not => {
                f.write_str("(not ")?;
                expr.fmt_raw_template(f)?;
                f.write_char(')')
            }
            Self::IsNone => {
                f.write_char('(')?;
                expr.fmt_raw_template(f)?;
                f.write_str(" is none)")
            }
            Self::IsNotNone => {
                f.write_char('(')?;
                expr.fmt_raw_template(f)?;
                f.write_str(" is not none)")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateExpression {
    Op(Rc<TemplateExpression>, TemplateOp, Rc<TemplateExpression>),
    Member(Rc<TemplateExpression>, &'static str),
    IfThenElse {
        r#if: Rc<TemplateExpression>,
        then: Rc<TemplateExpression>,
        r#else: Rc<TemplateExpression>,
    },
    Unary(TemplateUnaryOp, Rc<TemplateExpression>),
    Call(
        &'static str,
        Rc<[(Option<&'static str>, Rc<TemplateExpression>)]>,
    ),
    Pipe(Rc<TemplateExpression>, Rc<TemplateExpression>),
    Literal(Rc<str>),
    String(Rc<str>),
    ConstExpr(Rc<TemplateExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateControl {
    Assign(Rc<str>, Rc<TemplateExpression>),
    For(Rc<str>, Rc<TemplateExpression>, Rc<[TemplateBlock]>),
}
impl TemplateControl {
    fn fmt_raw_template(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("{% ")?;
        match self {
            Self::Assign(value, expr) => {
                f.write_str("set ")?;
                f.write_str(value)?;
                f.write_char('=')?;
                expr.fmt_raw_template(f)?;
            }
            Self::For(value, expr, body) => {
                f.write_fmt(format_args!("for {value} in "))?;
                expr.fmt_raw_template(f)?;
                f.write_str(" %}")?;
                Template::print_contents(body.iter().cloned(), f)?;
                f.write_str("{% endfor")?;
            }
        }
        f.write_str(" %}")
    }
    pub fn iter(&self) -> impl Iterator<Item = TemplateBlock> {
        let mut iter = vec![];
        match self {
            TemplateControl::Assign(_, expr) => iter.push(TemplateBlock::Expr(expr.clone()).into()),
            TemplateControl::For(_rc, rc1, rc2) => {
                iter.push(TemplateBlock::Expr(rc1.clone()).into());
                iter.append(&mut rc2.iter().cloned().collect());
            }
        }
        iter.into_iter()
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum TemplateBlock {
    Text(Rc<str>),
    Expr(Rc<TemplateExpression>),
    Control(Rc<TemplateControl>),
}

struct RecursiveIter<T, F: Fn(&TemplateBlock) -> Option<T>> {
    known: VecDeque<TemplateBlock>,
    return_filter: F,
}

impl<T: std::fmt::Debug, F: Fn(&TemplateBlock) -> Option<T>> Iterator for RecursiveIter<T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(item) = self.known.pop_front() {
            if let Some(ret) = (self.return_filter)(&item) {
                return Some(ret);
            } else {
                match item {
                    TemplateBlock::Text(_) => todo!(),
                    TemplateBlock::Expr(item) => {
                        for item in item.iter() {
                            self.known.push_back(item);
                        }
                    }
                    TemplateBlock::Control(item) => {
                        for item in item.iter() {
                            self.known.push_back(item);
                        }
                    }
                }
            }
            self.next()
        } else {
            None
        }
    }
}

#[derive(Default)]
pub struct Template {
    content: Vec<TemplateBlock>,
    state: Mutex<TemplateState>,
}

impl Clone for Template {
    fn clone(&self) -> Self {
        Template {
            content: self.content.clone(),
            state: Mutex::new(self.state.lock().unwrap().clone()),
        }
    }
}

impl From<Rc<TemplateExpression>> for Template {
    fn from(value: Rc<TemplateExpression>) -> Self {
        Template {
            content: vec![TemplateBlock::Expr(value.into())],
            ..Default::default()
        }
    }
}

impl Template {
    fn print_contents(
        content: impl IntoIterator<Item = TemplateBlock>,
        f: &mut Formatter<'_, '_>,
    ) -> std::fmt::Result {
        use TemplateBlock as B;
        for block in content.into_iter() {
            match block {
                B::Text(text) => f.write_str(&text)?,
                B::Control(expr) => {
                    expr.fmt_raw_template(f)?;
                }
                B::Expr(expr) => {
                    f.write_str("{{ ")?;
                    expr.fmt_raw_template(f)?;
                    f.write_str(" }}")?;
                }
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Template {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut state = self.state.lock().unwrap();
        let f = &mut Formatter {
            f,
            state: &mut state,
        };
        enum CacheStatus {
            Candidate,
            Cached,
        }
        let mut status = HashMap::new();
        let mut prefix = vec![];
        let iterator = RecursiveIter {
            known: self.content.iter().cloned().collect(),
            return_filter: |x| {
                if let TemplateBlock::Expr(ref expr) = x {
                    if let TemplateExpression::ConstExpr(ref const_expr) = **expr {
                        return Some(const_expr.clone());
                    }
                }
                None
            },
        };
        for expr in iterator {
            let expr: Rc<TemplateExpression> = expr.into();
            let const_entry = f.state.const_expressions.entry(expr.clone());
            if let std::collections::hash_map::Entry::Vacant(const_entry) = const_entry {
                status
                    .entry(expr.clone())
                    .and_modify(|status| {
                        if matches!(status, CacheStatus::Candidate) {
                            let var_name = TemplateState::new_var_in(&mut f.state.variables);
                            prefix.push(TemplateBlock::Control(
                                TemplateControl::Assign(var_name.clone(), expr.clone()).into(),
                            ));
                            const_entry.insert(TemplateExpression::Literal(var_name).into());
                            *status = CacheStatus::Cached
                        }
                    })
                    .or_insert_with(|| CacheStatus::Candidate);
            } else {
            }
        }

        Self::print_contents(prefix.into_iter().chain(self.content.iter().cloned()), f)
    }
}

impl Serialize for Template {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        to_string_serialize(self, serializer)
    }
}

impl TemplateExpression {
    fn fmt_raw_template(&self, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
        match self {
            Self::Op(rc, template_op, rc1) => {
                f.write_str("(")?;
                rc.fmt_raw_template(f)?;
                template_op.fmt_raw_template(f)?;
                rc1.fmt_raw_template(f)?;
                f.write_str(")")
            }
            Self::ConstExpr(expr) => f
                .state
                .const_expressions
                .get(expr)
                .unwrap_or(expr)
                .clone()
                .fmt_raw_template(f),
            Self::Unary(op, rc) => op.fmt_raw_template(f, rc),
            Self::Call(rc, rc1) => {
                f.write_str(rc)?;
                f.write_str("(")?;
                let mut first = true;
                for (arg_name, arg) in rc1.iter() {
                    if first {
                        first = false;
                    } else {
                        f.write_char(',')?;
                    }
                    if let Some(arg_name) = arg_name {
                        f.write_str(arg_name)?;
                        f.write_char('=')?;
                    }
                    arg.fmt_raw_template(f)?;
                }
                f.write_str(")")
            }
            Self::Member(rc, name) => {
                rc.fmt_raw_template(f)?;
                f.write_char('.')?;
                f.write_str(name)
            }
            Self::Pipe(rc, rc1) => {
                f.write_str("(")?;
                rc.fmt_raw_template(f)?;
                f.write_str("|")?;
                rc1.fmt_raw_template(f)?;
                f.write_str(")")
            }
            Self::IfThenElse { r#if, then, r#else } => {
                f.write_char('(')?;
                then.fmt_raw_template(f)?;
                f.write_str(" if ")?;
                r#if.fmt_raw_template(f)?;
                f.write_str(" else ")?;
                r#else.fmt_raw_template(f)?;
                f.write_char(')')
            }
            Self::Literal(rc) => f.write_str(rc),
            Self::String(rc) => f.write_fmt(format_args!("\"{}\"", rc)), // TODO: Escape?
                                                                         // Seems like overkill.
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = TemplateBlock> {
        let mut iter = vec![];
        match self {
            Self::Op(rc, _, rc1) | Self::Pipe(rc, rc1) => {
                iter.push(rc.clone());
                iter.push(rc1.clone());
            }
            Self::Member(rc, _) | Self::Unary(_, rc) | Self::ConstExpr(rc) => {
                iter.push(rc.clone());
            }
            Self::Call(_, rc) => {
                iter.append(&mut rc.iter().map(|(_, expr)| expr).cloned().collect());
            }
            Self::IfThenElse { r#if, then, r#else } => {
                iter.push(r#if.clone());
                iter.push(then.clone());
                iter.push(r#else.clone());
            }
            Self::Literal(_) | Self::String(_) => (),
        }
        iter.into_iter().map(|x| TemplateBlock::Expr(x).into())
    }
    fn to_raw_template(&self) -> Rc<str> {
        struct Print<'a>(&'a TemplateExpression);
        impl std::fmt::Display for Print<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt_raw_template(&mut Formatter {
                    f,
                    state: &mut Default::default(),
                })
            }
        }
        Print(self).to_string().into()
    }
}

#[derive(Clone)]
pub struct TemplateVariable(Rc<TemplateExpression>);
impl Deref for TemplateVariable {
    type Target = Rc<TemplateExpression>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone)]
pub struct TemplateNamespaceVariable(Rc<TemplateExpression>);

impl TemplateNamespaceVariable {
    pub fn member(self: &Self, name: &'static str) -> TemplateVariable {
        TemplateVariable(TemplateExpression::Member(self.0.clone(), name).into())
    }
}

impl TemplateState {
    fn new_var_in(variables: &mut HashSet<Rc<str>>) -> Rc<str> {
        let mut idx = variables.len();
        let name = loop {
            let name: Rc<str> = Rc::from(format!("v{idx}"));
            if variables.insert(name.clone()) {
                break name;
            }
            idx += 1;
        };
        name
    }
    fn new_var(&mut self) -> Rc<str> {
        Self::new_var_in(&mut self.variables)
    }
}

impl Template {
    pub fn assign_new(&mut self, expr: impl Into<Rc<TemplateExpression>>) -> TemplateVariable {
        let name = self.state.lock().unwrap().new_var();
        self.content.push(TemplateBlock::Control(
            TemplateControl::Assign(name.clone(), expr.into()).into(),
        ));
        TemplateVariable(TemplateExpression::Literal(name).into()).into()
    }
    pub fn assign(&mut self, name: &TemplateVariable, expr: impl Into<Rc<TemplateExpression>>) {
        self.content.push(TemplateBlock::Control(
            TemplateControl::Assign(name.0.to_raw_template(), expr.into()).into(),
        ))
    }
    pub fn assign_new_namespace(
        &mut self,
        expr: impl IntoIterator<Item = (&'static str, Rc<TemplateExpression>)>,
    ) -> TemplateNamespaceVariable {
        let expr = expr.into_iter();
        let args: Vec<_> = expr.map(|(name, value)| (Some(name), value)).collect();
        let TemplateVariable(variable) =
            self.assign_new(TemplateExpression::fun("namespace", args));
        TemplateNamespaceVariable(variable)
    }
    pub fn for_each<T>(
        &mut self,
        expr: impl Into<Rc<TemplateExpression>>,
        fun: impl FnOnce(&mut Self, TemplateVariable) -> T,
    ) -> T {
        let var_name = self.state.lock().unwrap().new_var();
        let mut template = Template {
            state: self.state.lock().unwrap().clone().into(),
            ..Default::default()
        };
        let result = fun(
            &mut template,
            TemplateVariable(TemplateExpression::Literal(var_name.clone()).into()),
        );
        self.content.push(TemplateBlock::Control(
            TemplateControl::For(var_name, expr.into(), template.content.into()).into(),
        ));
        result
    }
    pub fn expr(&mut self, content: impl Into<Rc<TemplateExpression>>) {
        self.content.push(TemplateBlock::Expr(content.into()))
    }
}

//impl From<&TemplateExpression> for Condition {
//    fn from(value: &TemplateExpression) -> Self {
//        value.to_condition()
//    }
//}

impl<T: Into<Rc<TemplateExpression>>> Add<T> for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn add(self, rhs: T) -> Self::Output {
        TemplateExpression::Op(self.clone().into(), TemplateOp::Add, rhs.into()).into()
    }
}

impl<T: Into<Rc<TemplateExpression>>> Sub<T> for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn sub(self, rhs: T) -> Self::Output {
        TemplateExpression::Op(self.clone().into(), TemplateOp::Sub, rhs.into()).into()
    }
}

impl<T: Into<Rc<TemplateExpression>>> Mul<T> for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn mul(self, rhs: T) -> Self::Output {
        TemplateExpression::Op(self.clone().into(), TemplateOp::Mul, rhs.into()).into()
    }
}

impl<T: Into<Rc<TemplateExpression>>> Div<T> for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn div(self, rhs: T) -> Self::Output {
        TemplateExpression::Op(self.clone().into(), TemplateOp::Div, rhs.into()).into()
    }
}

impl<T: Into<Rc<TemplateExpression>>> Rem<T> for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn rem(self, rhs: T) -> Self::Output {
        TemplateExpression::Op(self.clone().into(), TemplateOp::Rem, rhs.into()).into()
    }
}

impl Add for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn add(self, rhs: Self) -> Self::Output {
        self.add(Rc::from(rhs.clone()))
    }
}

impl Sub for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn sub(self, rhs: Self) -> Self::Output {
        self.sub(Rc::from(rhs.clone()))
    }
}

impl Mul for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn mul(self, rhs: Self) -> Self::Output {
        self.mul(Rc::from(rhs.clone()))
    }
}

impl Div for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn div(self, rhs: Self) -> Self::Output {
        self.div(Rc::from(rhs.clone()))
    }
}

impl Rem for &TemplateExpression {
    type Output = Rc<TemplateExpression>;

    fn rem(self, rhs: Self) -> Self::Output {
        self.rem(Rc::from(rhs.clone()))
    }
}

impl TemplateExpression {
    fn raise_constexpr(
        self: &Rc<Self>,
        fun: impl Fn(Rc<TemplateExpression>) -> Rc<TemplateExpression>,
    ) -> Rc<TemplateExpression> {
        match &**self {
            TemplateExpression::ConstExpr(x) => fun(x.clone()).mark_const_expr(),
            _ => fun(self.clone()),
        }
    }
    pub fn bool(value: bool) -> Rc<TemplateExpression> {
        Self::Literal(if value { "True" } else { "False" }.into()).into()
    }
    pub fn string(value: impl Into<Rc<str>>) -> Rc<TemplateExpression> {
        Self::String(value.into()).into()
    }
    fn op(
        self: Rc<Self>,
        op: TemplateOp,
        rhs: impl Into<Rc<TemplateExpression>>,
    ) -> Rc<TemplateExpression> {
        Self::Op(self, op, rhs.into()).into()
    }
    fn unary(self: Rc<Self>, op: TemplateUnaryOp) -> Rc<TemplateExpression> {
        Self::Unary(op, self).into()
    }
    pub fn gt(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Rem, rhs.into())
    }
    pub fn ge(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Ge, rhs.into())
    }
    pub fn lt(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Lt, rhs.into())
    }
    pub fn le(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Le, rhs.into())
    }
    pub fn eq(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Eq, rhs.into())
    }
    pub fn and(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::And, rhs.into())
    }
    pub fn or(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Or, rhs.into())
    }
    pub fn is_none(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.unary(TemplateUnaryOp::IsNone)
    }
    pub fn is_not_none(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.unary(TemplateUnaryOp::IsNotNone)
    }
    pub fn literal(constant: impl ToString) -> Rc<TemplateExpression> {
        TemplateExpression::Literal(constant.to_string().into()).into()
    }
    pub fn fun<I: Into<Rc<TemplateExpression>>>(
        fun: &'static str,
        args: impl IntoIterator<Item = (Option<&'static str>, I)>,
    ) -> Rc<TemplateExpression> {
        TemplateExpression::Call(
            fun,
            args.into_iter()
                .map(|(name, arg)| (name, arg.into()))
                .collect(),
        )
        .into()
    }
    fn call_on_self(self: Rc<Self>, fun: &'static str) -> Rc<TemplateExpression> {
        Self::fun(fun, [(None, self)])
    }
    pub fn to_float(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_constexpr(|this| this.call_on_self("float"))
    }
    pub fn to_int(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_constexpr(|this| this.call_on_self("int"))
    }
    pub fn pipe_to(
        self: Rc<Self>,
        rhs: impl Into<Rc<TemplateExpression>>,
    ) -> Rc<TemplateExpression> {
        Self::Pipe(self, rhs.into()).into()
    }
    pub fn round(self: Rc<Self>, precision: usize) -> Rc<TemplateExpression> {
        self.pipe_to(Self::fun("round", [(None, Self::literal(precision))]))
    }
    pub fn abs(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.pipe_to(Self::literal("abs"))
    }
    pub fn now() -> Rc<TemplateExpression> {
        Self::fun::<Self>("now", []).mark_const_expr()
    }
    pub fn fold<I: Into<Rc<TemplateExpression>>>(
        data: impl IntoIterator<Item = I>,
        mut op: impl FnMut(Rc<TemplateExpression>, Rc<TemplateExpression>) -> Rc<TemplateExpression>,
    ) -> Option<Rc<TemplateExpression>> {
        let iter = data.into_iter();
        let mut result: Option<Rc<_>> = None;
        for item in iter {
            result = Some(if let Some(prev) = result {
                op(prev, item.into())
            } else {
                item.into()
            })
        }
        result
    }
    pub fn mark_const_expr(self: Rc<TemplateExpression>) -> Rc<TemplateExpression> {
        TemplateExpression::ConstExpr(self).into()
    }
    pub fn sum<I: Into<Rc<TemplateExpression>>>(
        data: impl IntoIterator<Item = I>,
    ) -> Option<Rc<TemplateExpression>> {
        Self::fold(data, |v1, v2| &*v1 + v2)
    }
    pub fn this_automation_last_trigger() -> Rc<TemplateExpression> {
        Self::literal("this.attributes.last_triggered")
    }
    pub fn range(
        low: impl Into<Rc<TemplateExpression>>,
        high: impl Into<Rc<TemplateExpression>>,
    ) -> Rc<TemplateExpression> {
        Self::fun("range", [(None, low.into()), (None, high.into())])
    }
    pub fn timedelta(
        hours: impl Into<Rc<TemplateExpression>>,
        minutes: impl Into<Rc<TemplateExpression>>,
        seconds: impl Into<Rc<TemplateExpression>>,
    ) -> Rc<TemplateExpression> {
        Self::fun(
            "timedelta",
            [
                (Some("hours"), hours.into()),
                (Some("minutes"), minutes.into()),
                (Some("seconds"), seconds.into()),
            ],
        )
    }
    pub fn if_then_else(
        r#if: impl Into<Rc<TemplateExpression>>,
        then: impl Into<Rc<TemplateExpression>>,
        r#else: impl Into<Rc<TemplateExpression>>,
    ) -> Rc<TemplateExpression> {
        TemplateExpression::IfThenElse {
            r#if: r#if.into(),
            then: then.into(),
            r#else: r#else.into(),
        }
        .into()
    }
}
impl TemplateExpression {
    pub fn min<I: Into<Rc<TemplateExpression>>>(
        values: impl IntoIterator<Item = I>,
    ) -> Option<Rc<TemplateExpression>> {
        let mut values = values.into_iter().peekable();
        if values.peek().is_none() {
            None
        } else {
            Some(Self::fun(
                "min",
                values.map(|x| (None, x.into())).collect::<Vec<_>>(),
            ))
        }
    }

    pub fn max<I: Into<Rc<TemplateExpression>>>(
        values: impl IntoIterator<Item = I>,
    ) -> Option<Rc<TemplateExpression>> {
        let mut values = values.into_iter().peekable();
        if values.peek().is_none() {
            None
        } else {
            Some(Self::fun(
                "max",
                values.map(|x| (None, x.into())).collect::<Vec<_>>(),
            ))
        }
    }

    pub fn average(
        values: impl Into<Rc<[Rc<TemplateExpression>]>>,
    ) -> Option<Rc<TemplateExpression>> {
        let values = values.into();
        let count = values.len();
        let mut sum: Option<Rc<Self>> = None;
        for value in &values[..] {
            sum = if let Some(old) = sum {
                Some(&*old + value.clone())
            } else {
                Some(value.clone())
            };
        }
        if let Some(sum) = sum {
            Some(&*sum / Self::literal(count))
        } else {
            None
        }
    }
}
