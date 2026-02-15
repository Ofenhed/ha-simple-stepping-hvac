use serde::Serialize;

use crate::{
    entity_id::EntityId,
    types::{to_string_serialize, LinkedTree},
};
use std::{
    borrow::Cow,
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
    fn invert(self) -> Option<Self> {
        match self {
            Self::Gt => Some(Self::Le),
            Self::Ge => Some(Self::Lt),
            Self::Lt => Some(Self::Ge),
            Self::Le => Some(Self::Gt),
            Self::Eq => Some(Self::Ne),
            Self::Ne => Some(Self::Eq),
            _ => None,
        }
    }
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
    Neg,
    IsNone,
    IsNotNone,
    Log2,
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
    fn fmt_raw_template(
        &self,
        f: &mut Formatter,
        expr: &TemplateExpression,
        atomize: bool,
    ) -> std::fmt::Result {
        match self {
            Self::Not => {
                if atomize {
                    f.write_char('(')?;
                }
                f.write_str("not ")?;
                expr.fmt_raw_template(f, true)?;
                if atomize {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::IsNone => {
                if atomize {
                    f.write_char('(')?;
                }
                expr.fmt_raw_template(f, true)?;
                f.write_str(" is none")?;
                if atomize {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::IsNotNone => {
                if atomize {
                    f.write_char('(')?;
                }
                expr.fmt_raw_template(f, true)?;
                f.write_str(" is not none")?;
                if atomize {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::Neg => {
                if atomize {
                    f.write_char('(')?;
                }
                f.write_char('-')?;
                expr.fmt_raw_template(f, true)?;
                if atomize {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::Log2 => {
                f.write_str("log(")?;
                expr.fmt_raw_template(f, false)?;
                f.write_str(", 2)")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateExpression {
    Op(Rc<TemplateExpression>, TemplateOp, Rc<TemplateExpression>),
    Member(Rc<TemplateExpression>, Cow<'static, str>),
    IfThenElse {
        r#if: Rc<TemplateExpression>,
        then: Rc<TemplateExpression>,
        r#else: Rc<TemplateExpression>,
    },
    Unary(TemplateUnaryOp, Rc<TemplateExpression>),
    Call(
        &'static str,
        Rc<[(Option<Cow<'static, str>>, Rc<TemplateExpression>)]>,
    ),
    Pipe(Rc<TemplateExpression>, Rc<TemplateExpression>),
    Literal(Rc<str>),
    String(Rc<str>),
    ConstExpr(Rc<TemplateExpression>),
    NamedConstExpr(Rc<str>, Rc<TemplateExpression>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateControl {
    Assign(Rc<str>, Rc<TemplateExpression>),
    For(Rc<str>, Rc<TemplateExpression>, Rc<[TemplateBlock]>),
}
impl TemplateControl {
    fn fmt_raw_template(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str("{%\n  ")?;
        match self {
            Self::Assign(value, expr) => {
                f.write_str("set ")?;
                f.write_str(value)?;
                f.write_char('=')?;
                expr.fmt_raw_template(f, false)?;
            }
            Self::For(value, expr, body) => {
                f.write_fmt(format_args!("for {value} in "))?;
                expr.fmt_raw_template(f, false)?;
                f.write_str("\n%}")?;
                Template::print_contents(body.iter().cloned(), f)?;
                f.write_str("{%\n  endfor")?;
            }
        }
        f.write_str("\n%}")
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
    Optional(Rc<TemplateBlock>),
}

struct RecursiveIter<T, S: Clone, F: Fn(Option<&S>, &TemplateBlock) -> Option<(S, T)>> {
    known: VecDeque<(Option<S>, TemplateBlock)>,
    return_filter: F,
}

impl<T, S: Clone, F: Fn(Option<&S>, &TemplateBlock) -> Option<(S, T)>> RecursiveIter<T, S, F> {
    pub fn new(items: impl IntoIterator<Item = TemplateBlock>, filter: F) -> Self {
        Self {
            known: items.into_iter().map(|x| (None, x)).collect(),
            return_filter: filter,
        }
    }
}

impl<T: std::fmt::Debug, S: Clone, F: Fn(Option<&S>, &TemplateBlock) -> Option<(S, T)>> Iterator
    for RecursiveIter<T, S, F>
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let mut returned = None;
        if let Some((state, item)) = self.known.pop_front() {
            let parent_state = if let Some((new_parent_state, ret)) =
                (self.return_filter)(state.as_ref(), &item)
            {
                returned = Some(ret);
                Some(new_parent_state)
            } else {
                state
            };
            match item {
                TemplateBlock::Text(_) => (),
                TemplateBlock::Expr(item) => {
                    for item in item.iter() {
                        self.known.push_back((parent_state.clone(), item));
                    }
                }
                TemplateBlock::Optional(item) => self
                    .known
                    .push_back((parent_state.clone(), Rc::unwrap_or_clone(item))),
                TemplateBlock::Control(item) => {
                    for item in item.iter() {
                        self.known.push_back((parent_state.clone(), item));
                    }
                }
            }
            returned.or_else(|| self.next())
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
        fn print_block(block: &B, f: &mut Formatter<'_, '_>) -> std::fmt::Result {
            match block {
                B::Text(text) => f.write_str(&text),
                B::Control(expr) => expr.fmt_raw_template(f),
                B::Optional(block) => print_block(&block, f),
                B::Expr(expr) => {
                    f.write_str("{{ ")?;
                    expr.fmt_raw_template(f, false)?;
                    f.write_str(" }}")
                }
            }
        }
        for block in content.into_iter() {
            print_block(&block, f)?;
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
        let iterator = RecursiveIter::new(
            self.content.iter().cloned(),
            |parent: Option<&(
                bool,
                Rc<LinkedTree<(Option<Rc<str>>, Rc<TemplateExpression>)>>,
            )>,
             x| {
                if let (Some((false, parent)), TemplateBlock::Optional(_)) = (parent, x) {
                    return Some(((true, parent.clone()), None));
                }
                if let TemplateBlock::Expr(ref expr) = x {
                    if let Some((name, const_expr)) = match **expr {
                        TemplateExpression::ConstExpr(ref const_expr) => Some((None, const_expr)),
                        TemplateExpression::NamedConstExpr(ref name, ref const_expr) => {
                            Some((Some(name), const_expr))
                        }
                        _ => None,
                    } {
                        let state = if let Some((optional, parent)) = parent {
                            (
                                *optional,
                                parent.new_child((name.cloned(), const_expr.clone())),
                            )
                        } else {
                            (false, LinkedTree::new((name.cloned(), const_expr.clone())))
                        };
                        return Some((state.clone(), Some(state)));
                    }
                }
                None
            },
        );
        let items = iterator.collect::<Vec<_>>();
        let mut setters: Vec<(Rc<TemplateExpression>, Option<Rc<str>>, i32)> = vec![];

        for state in items.into_iter() {
            let Some((optional, state)) = state else {
                continue;
            };
            while let Some(expr) = state.take_leaf() {
                if let Some((_, name, counter)) = setters
                    .iter_mut()
                    .find(|(expression, ..)| expression == &expr.1)
                {
                    if !optional {
                        *counter += 1;
                    }
                    if let (previous @ None, Some(new_name)) = (name, expr.0.clone()) {
                        *previous = Some(new_name.clone());
                    }
                } else {
                    setters.push(((expr.1).clone(), expr.0.clone(), 0));
                }
            }
            let expr: Rc<TemplateExpression> = state.1.clone();
            if let Some((_, _, counter)) = setters
                .iter_mut()
                .find(|(expression, ..)| expression == &expr)
            {
                if !optional {
                    *counter += 1;
                }
            } else {
                setters.push((expr, state.0.clone(), 0));
            }
        }
        //iterator.for_each(top_down_add);
        let mut prefix = vec![];
        for (expr, name, count) in setters.into_iter() {
            if count == 0 {
                continue;
            }
            let var_name = TemplateState::new_var_in(name.as_deref(), &mut f.state.variables);
            prefix.push(TemplateBlock::Control(
                TemplateControl::Assign(var_name.clone(), expr.clone().clone()).into(),
            ));
            f.state
                .const_expressions
                .insert(expr, TemplateExpression::Literal(var_name).into());
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
    fn fmt_raw_template(&self, f: &mut Formatter<'_, '_>, atomize: bool) -> std::fmt::Result {
        match self {
            Self::Op(rc, template_op, rc1) => {
                if atomize {
                    f.write_str("(")?;
                }
                rc.fmt_raw_template(f, true)?;
                template_op.fmt_raw_template(f)?;
                rc1.fmt_raw_template(f, true)?;
                if atomize {
                    f.write_str(")")?;
                }
                Ok(())
            }
            Self::ConstExpr(expr) | Self::NamedConstExpr(_, expr) => f
                .state
                .const_expressions
                .get(expr)
                .unwrap_or(expr)
                .clone()
                .fmt_raw_template(f, atomize),
            Self::Unary(op, rc) => op.fmt_raw_template(f, rc, atomize),
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
                    arg.fmt_raw_template(f, false)?;
                }
                f.write_str(")")
            }
            Self::Member(rc, name) => {
                rc.fmt_raw_template(f, true)?;
                f.write_char('.')?;
                f.write_str(name)
            }
            Self::Pipe(rc, rc1) => {
                if atomize {
                    f.write_str("(")?;
                }
                rc.fmt_raw_template(f, true)?;
                f.write_str("|")?;
                rc1.fmt_raw_template(f, true)?;
                if atomize {
                    f.write_str(")")?;
                }
                Ok(())
            }
            Self::IfThenElse { r#if, then, r#else } => {
                if atomize {
                    f.write_char('(')?;
                }
                then.fmt_raw_template(f, true)?;
                f.write_str(" if ")?;
                r#if.fmt_raw_template(f, true)?;
                f.write_str(" else ")?;
                r#else.fmt_raw_template(f, true)?;
                if atomize {
                    f.write_char(')')?;
                }
                Ok(())
            }
            Self::Literal(rc) => f.write_str(rc),
            Self::String(rc) => f.write_fmt(format_args!("\"{}\"", rc)), // TODO: Escape?
                                                                         // Seems like overkill.
        }
    }
    pub fn iter(&self) -> impl Iterator<Item = TemplateBlock> {
        let mut iter = vec![];
        let mut push_expr = |expr| iter.push(TemplateBlock::Expr(expr));
        match self {
            Self::Op(rc, _, rc1) | Self::Pipe(rc, rc1) => {
                push_expr(rc.clone());
                push_expr(rc1.clone());
            }
            Self::Member(rc, _)
            | Self::Unary(_, rc)
            | Self::ConstExpr(rc)
            | Self::NamedConstExpr(_, rc) => {
                push_expr(rc.clone());
            }
            Self::Call(_, rc) => {
                rc.iter().for_each(|(_, expr)| push_expr(expr.clone()));
            }
            Self::IfThenElse { r#if, then, r#else } => {
                push_expr(r#if.clone());
                iter.push(TemplateBlock::Optional(
                    TemplateBlock::Expr(r#then.clone()).into(),
                ));
                iter.push(TemplateBlock::Optional(
                    TemplateBlock::Expr(r#else.clone()).into(),
                ));
            }
            Self::Literal(_) | Self::String(_) => (),
        }
        iter.into_iter()
    }
    fn to_raw_template(&self) -> Rc<str> {
        struct Print<'a>(&'a TemplateExpression);
        impl std::fmt::Display for Print<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.0.fmt_raw_template(
                    &mut Formatter {
                        f,
                        state: &mut Default::default(),
                    },
                    false,
                )
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

impl TemplateVariable {
    pub fn member(self: &Self, name: Cow<'static, str>) -> TemplateVariable {
        TemplateVariable(self.0.clone().member(name))
    }
}

impl TemplateState {
    #[must_use]
    fn new_var_in(prefix: Option<&str>, variables: &mut HashSet<Rc<str>>) -> Rc<str> {
        let (mut idx, prefix) = if let Some(prefix) = prefix {
            (0, prefix)
        } else {
            (variables.len(), "v")
        };
        let prefix = EntityId::encode_string(prefix);
        let name = loop {
            let name: Rc<str> = if idx == 0 {
                prefix.clone().into()
            } else {
                Rc::from(format!("{prefix}{idx}"))
            };
            if variables.insert(name.clone()) {
                break name;
            }
            idx += 1;
        };
        name
    }
    #[must_use]
    fn new_var(&mut self) -> Rc<str> {
        Self::new_var_in(None, &mut self.variables)
    }
}

impl Template {
    #[must_use]
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
    #[must_use]
    pub fn assign_new_namespace(
        &mut self,
        expr: impl IntoIterator<Item = (Cow<'static, str>, Rc<TemplateExpression>)>,
    ) -> TemplateVariable {
        let expr = expr.into_iter();
        let args: Vec<_> = expr.map(|(name, value)| (Some(name), value)).collect();
        let TemplateVariable(variable) =
            self.assign_new(TemplateExpression::fun("namespace", args));
        TemplateVariable(variable)
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
    pub fn text(&mut self, content: impl ToString) {
        self.content
            .push(TemplateBlock::Text(content.to_string().into()))
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
    fn raise_named_constexpr(
        self: &Rc<Self>,
        fun: impl Fn(Rc<TemplateExpression>, &mut Option<Rc<str>>) -> Rc<TemplateExpression>,
    ) -> Rc<TemplateExpression> {
        let mut name = if let TemplateExpression::NamedConstExpr(name, _) = &**self {
            Some(name.clone())
        } else {
            None
        };
        match &**self {
            TemplateExpression::ConstExpr(x) | TemplateExpression::NamedConstExpr(_, x) => {
                let new = fun(x.clone(), &mut name);
                if let Some(name) = name {
                    new.mark_named_const_expr(name)
                } else {
                    new.mark_const_expr()
                }
            }
            _ => fun(self.clone(), &mut name),
        }
    }
    #[allow(unused)]
    fn raise_constexpr(
        self: &Rc<Self>,
        fun: impl Fn(Rc<TemplateExpression>) -> Rc<TemplateExpression>,
    ) -> Rc<TemplateExpression> {
        self.raise_named_constexpr(|expr, name| {
            *name = None;
            fun(expr)
        })
    }
    pub fn bool(value: bool) -> Rc<TemplateExpression> {
        Self::Literal(if value { "True" } else { "False" }.into()).into()
    }
    pub fn member(self: &Rc<Self>, name: Cow<'static, str>) -> Rc<TemplateExpression> {
        Self::Member(self.clone(), name).into()
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
        self.op(TemplateOp::Gt, rhs.into())
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
    pub fn ne(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Ne, rhs.into())
    }
    pub fn and(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::And, rhs.into())
    }
    pub fn or(self: Rc<Self>, rhs: impl Into<Rc<TemplateExpression>>) -> Rc<TemplateExpression> {
        self.op(TemplateOp::Or, rhs.into())
    }
    pub fn not(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_named_constexpr(|value, name| match &*value {
            Self::Op(this, op, rhs) => {
                *name = None;
                op.invert()
                    .map(|op| Self::Op(this.clone(), op, rhs.clone()).into())
                    .unwrap_or(value.unary(TemplateUnaryOp::Not))
                    .into()
            }
            _ => {
                name.as_mut()
                    .map(|name| *name = format!("not_{name}").into());
                value.unary(TemplateUnaryOp::Not)
            }
        })
    }
    pub fn neg(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_named_constexpr(|value, name| match &*value {
            Self::Unary(TemplateUnaryOp::Neg, new_self) => {
                *name = None;
                new_self.clone()
            }
            _ => {
                name.as_mut()
                    .map(|name| *name = format!("neg_{name}").into());
                value.unary(TemplateUnaryOp::Neg)
            }
        })
    }
    pub fn log2(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_named_constexpr(|value, name| {
            name.as_mut()
                .map(|name| *name = format!("log2_{name}").into());
            value.unary(TemplateUnaryOp::Log2)
        })
    }
    pub fn is_none(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.unary(TemplateUnaryOp::IsNone)
    }
    pub fn is_not_none(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.unary(TemplateUnaryOp::IsNotNone)
    }
    pub fn literal(constant: impl ToString) -> Rc<TemplateExpression> {
        let constant = constant.to_string();
        let mut checker = constant.chars();
        let mut has_dot = false;
        let mut has_e = false;
        let constant = match checker.next() {
            Some('-')
                if checker
                    .find(|char| match char {
                        '0'..'9' => false,
                        '.' if has_dot => true,
                        '.' => {
                            has_dot = true;
                            false
                        }
                        'e' | 'E' if has_e => true,
                        'e' | 'E' => {
                            has_e = true;
                            false
                        }
                        _ => true,
                    })
                    .is_none() =>
            {
                format!("({constant})")
            }
            _ => constant,
        };
        TemplateExpression::Literal(constant.to_string().into()).into()
    }
    pub fn fun<I: Into<Rc<TemplateExpression>>>(
        fun: &'static str,
        args: impl IntoIterator<Item = (Option<Cow<'static, str>>, I)>,
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
        self.raise_named_constexpr(|this, _| this.pipe_to(Self::literal("float")))
    }
    pub fn to_int(self: Rc<Self>) -> Rc<TemplateExpression> {
        self.raise_named_constexpr(|this, _| this.pipe_to(Self::literal("int")))
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
        match &*self {
            Self::ConstExpr(_) | Self::NamedConstExpr(..) => self.clone(),
            _ => Self::ConstExpr(self).into(),
        }
    }
    pub fn mark_named_const_expr(
        self: Rc<TemplateExpression>,
        name: impl Into<Rc<str>>,
    ) -> Rc<TemplateExpression> {
        match &*self {
            Self::ConstExpr(inner) | Self::NamedConstExpr(_, inner) => {
                Self::NamedConstExpr(name.into(), inner.clone()).into()
            }
            _ => Self::NamedConstExpr(name.into(), self).into(),
        }
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
                (Some(Cow::Borrowed("hours")), hours.into()),
                (Some(Cow::Borrowed("minutes")), minutes.into()),
                (Some(Cow::Borrowed("seconds")), seconds.into()),
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
