//! Support for registering and creating XPath functions.

use crate::dom::bindings::root::DomRoot;
use crate::dom::node::Node;

use super::context;
use super::eval_value::{str_to_num, Value};
use std::borrow::ToOwned;
use std::cmp::Ordering;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::iter;
use std::ops::Index;
use thiserror::Error as ThisError;

/// Types that can be used as XPath functions.
pub trait Function<'c, 'd> {
    /// Evaluate this function in a specific context with a specific
    /// set of arguments.
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error>;
}

/// Represents the kind of an XPath value without carrying a value.
#[derive(Debug, Copy, Clone, PartialEq, Hash)]
pub enum ArgumentType {
    Boolean,
    Number,
    String,
    Nodeset,
}

impl<'c, 'd> From<&'d Value> for ArgumentType {
    fn from(other: &'d Value) -> ArgumentType {
        match *other {
            Value::Boolean(..) => ArgumentType::Boolean,
            Value::Number(..) => ArgumentType::Number,
            Value::String(..) => ArgumentType::String,
            Value::Nodeset(..) => ArgumentType::Nodeset,
        }
    }
}

/// The errors that may occur while evaluating a function
#[derive(Debug, ThisError, Clone, PartialEq, Hash)]
pub enum Error {
    #[error("too many arguments, expected {expected:?} but had {actual:?}")]
    TooManyArguments { expected: usize, actual: usize },
    #[error("not enough arguments, expected {expected:?} but had {actual:?}")]
    NotEnoughArguments { expected: usize, actual: usize },
    #[error("attempted to use an argument that was not present")]
    ArgumentMissing,
    #[error("argument was expected to be a nodeset but was a {actual:?}")]
    ArgumentNotANodeset { actual: ArgumentType },
    #[error("could not evaluate function: {what:?}")]
    Other { what: String },
}

impl<'c> Error {
    fn not_a_nodeset(actual: &Value) -> Error {
        Error::ArgumentNotANodeset {
            actual: actual.into(),
        }
    }
}

/// Provides common utility functions for dealing with function
/// argument lists.
pub struct Args<'c>(pub Vec<&'c Value>);

impl<'c, 'd> Args<'c> {
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Ensures that there are at least the requested number of arguments.
    pub fn at_least(&self, minimum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual < minimum {
            Err(Error::NotEnoughArguments {
                expected: minimum,
                actual,
            })
        } else {
            Ok(())
        }
    }

    /// Ensures that there are no more than the requested number of arguments.
    pub fn at_most(&self, maximum: usize) -> Result<(), Error> {
        let actual = self.0.len();
        if actual > maximum {
            Err(Error::TooManyArguments {
                expected: maximum,
                actual,
            })
        } else {
            Ok(())
        }
    }

    /// Ensures that there are exactly the requested number of arguments.
    pub fn exactly(&self, expected: usize) -> Result<(), Error> {
        let actual = self.0.len();
        match actual.cmp(&expected) {
            Ordering::Less => Err(Error::NotEnoughArguments { expected, actual }),
            Ordering::Greater => Err(Error::TooManyArguments { expected, actual }),
            Ordering::Equal => Ok(()),
        }
    }

    /// Converts all the arguments into strings.
    fn into_strings(self) -> Vec<String> {
        self.0
            .into_iter()
            .map(|val| Value::into_string(*val))
            .collect()
    }

    /// Removes the **last** argument and ensures it is a boolean. If
    /// the argument is not a boolean, it is converted to one.
    pub fn pop_boolean(&mut self) -> Result<bool, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_boolean())
    }

    /// Removes the **last** argument and ensures it is a number. If
    /// the argument is not a number, it is converted to one.
    pub fn pop_number(&mut self) -> Result<f64, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_number())
    }

    /// Removes the **last** argument and ensures it is a string. If
    /// the argument is not a string, it is converted to one.
    pub fn pop_string(&mut self) -> Result<String, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        Ok(v.into_string())
    }

    /// Removes the **last** argument and ensures it is a nodeset. If
    /// the argument is not a nodeset, a type mismatch error is
    /// returned.
    pub fn pop_nodeset(&mut self) -> Result<&Vec<DomRoot<Node>>, Error> {
        let v = self.0.pop().ok_or(Error::ArgumentMissing)?;
        match v {
            Value::Nodeset(v) => Ok(v),
            a => Err(Error::not_a_nodeset(&a)),
        }
    }

    /// Removes the **last** argument. If no argument is present, the
    /// context node is returned as a nodeset.
    fn pop_value_or_context_node(&mut self, context: &context::EvaluationCtx) -> &Value {
        self.0
            .pop()
            .unwrap_or_else(|| &Value::Nodeset(vec![context.context_node]))
    }

    /// Removes the **last** argument if it is a string. If no
    /// argument is present, the context node is converted to a string
    /// and returned. If there is an argument but it is not a string,
    /// it is converted to one.
    fn pop_string_value_or_context_node(&mut self, context: &context::EvaluationCtx) -> String {
        self.0
            .pop()
            .map(|val| Value::into_string(*val))
            .unwrap_or_else(|| context.context_node.string_value())
    }

    /// Removes the **last** argument if it is a nodeset. If no
    /// argument is present, the context node is added to a nodeset
    /// and returned. If there is an argument but it is not a nodeset,
    /// a type mismatch error is returned.
    fn pop_nodeset_or_context_node(
        &mut self,
        context: &context::EvaluationCtx,
    ) -> Result<&Nodeset<'c>, Error> {
        match self.0.pop() {
            Some(Value::Nodeset(ns)) => Ok(ns),
            Some(arg) => Err(Error::not_a_nodeset(&arg)),
            None => Ok(&Value::Nodeset(vec![context.context_node.clone()])),
        }
    }
}

impl<'c, 'd> Index<usize> for Args<'c> {
    type Output = Value;

    fn index(&self, index: usize) -> &Value {
        self.0.index(index)
    }
}

struct Last;

impl<'c, 'd> Function<'c, 'd> for Last {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(&Value::Number(context.size as f64))
    }
}

struct Position;

impl<'c, 'd> Function<'c, 'd> for Position {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(&Value::Number(context.position as f64))
    }
}

struct Count;

impl<'c, 'd> Function<'c, 'd> for Count {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_nodeset()?;
        Ok(&Value::Number(arg.size() as f64))
    }
}

struct LocalName;

impl<'c, 'd> Function<'c, 'd> for LocalName {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.expanded_name())
            .map(|q| q.local_part())
            .unwrap_or("");
        Ok(&Value::String(name.to_owned()))
    }
}

struct NamespaceUri;

impl<'c, 'd> Function<'c, 'd> for NamespaceUri {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.expanded_name())
            .and_then(|q| q.namespace_uri())
            .unwrap_or("");
        Ok(&Value::String(name.to_owned()))
    }
}

struct Name;

impl<'c, 'd> Function<'c, 'd> for Name {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_nodeset_or_context_node(context)?;
        let name = arg
            .document_order_first()
            .and_then(|n| n.prefixed_name())
            .unwrap_or_else(String::new);
        Ok(&Value::String(name))
    }
}

struct StringFn;

impl<'c, 'd> Function<'c, 'd> for StringFn {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_value_or_context_node(context);
        Ok(&Value::String(arg.string()))
    }
}

struct Concat;

impl<'c, 'd> Function<'c, 'd> for Concat {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.at_least(2)?;
        let args = args.into_strings();
        Ok(&Value::String(args.concat()))
    }
}

struct TwoStringPredicate(fn(&str, &str) -> bool);

impl<'c, 'd> Function<'c, 'd> for TwoStringPredicate {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(2)?;
        let args = args.into_strings();
        let v = self.0(&args[0], &args[1]);
        Ok(&Value::Boolean(v))
    }
}

fn starts_with() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool {
        str::starts_with(a, b)
    }
    TwoStringPredicate(imp)
}
fn contains() -> TwoStringPredicate {
    fn imp(a: &str, b: &str) -> bool {
        str::contains(a, b)
    }
    TwoStringPredicate(imp)
}

struct SubstringCommon(for<'s> fn(&'s str, &'s str) -> &'s str);

impl<'c, 'd> Function<'c, 'd> for SubstringCommon {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(2)?;
        let args = args.into_strings();
        let s = self.0(&args[0], &args[1]);
        Ok(&Value::String(s.to_owned()))
    }
}

fn substring_before() -> SubstringCommon {
    fn inner<'a>(haystack: &'a str, needle: &'a str) -> &'a str {
        match haystack.find(needle) {
            Some(pos) => &haystack[..pos],
            None => "",
        }
    }
    SubstringCommon(inner)
}

fn substring_after() -> SubstringCommon {
    fn inner<'a>(haystack: &'a str, needle: &'a str) -> &'a str {
        match haystack.find(needle) {
            Some(pos) => &haystack[pos + needle.len()..],
            None => "",
        }
    }
    SubstringCommon(inner)
}

struct Substring;

impl<'c, 'd> Function<'c, 'd> for Substring {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_least(2)?;
        args.at_most(3)?;

        let len = if args.len() == 3 {
            let len = args.pop_number()?;
            round_ties_to_positive_infinity(len)
        } else {
            ::std::f64::INFINITY
        };

        let start = args.pop_number()?;
        let start = round_ties_to_positive_infinity(start);
        let s = args.pop_string()?;

        let chars = s.chars().enumerate();
        let selected_chars = chars
            .filter_map(|(p, s)| {
                let p = (p + 1) as f64; // 1-based indexing
                if p >= start && p < start + len {
                    Some(s)
                } else {
                    None
                }
            })
            .collect();

        Ok(&Value::String(selected_chars))
    }
}

struct StringLength;

impl<'c, 'd> Function<'c, 'd> for StringLength {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_string_value_or_context_node(context);
        Ok(&Value::Number(arg.chars().count() as f64))
    }
}

struct NormalizeSpace;

impl<'c, 'd> Function<'c, 'd> for NormalizeSpace {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_string_value_or_context_node(context);

        fn is_xml_whitespace(ch: char) -> bool {
            matches!(ch, '\x20' | '\x09' | '\x0D' | '\x0A')
        }

        // TODO: research itertools or another pure-iterator solution
        let s: Vec<_> = arg
            .split(is_xml_whitespace)
            .filter(|s| !s.is_empty())
            .collect();
        let s = s.join(" ");
        Ok(&Value::String(s))
    }
}

struct Translate;

impl<'c, 'd> Function<'c, 'd> for Translate {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.exactly(3)?;

        let to = args.pop_string()?;
        let from = args.pop_string()?;
        let s = args.pop_string()?;

        let mut replacements = HashMap::new();
        let pairs = from
            .chars()
            .zip(to.chars().map(Some).chain(iter::repeat(None)));
        for (from, to) in pairs {
            if let Entry::Vacant(entry) = replacements.entry(from) {
                entry.insert(to);
            }
        }

        let s = s
            .chars()
            .filter_map(|c| replacements.get(&c).cloned().unwrap_or(Some(c)))
            .collect();

        Ok(&Value::String(s))
    }
}

struct BooleanFn;

impl<'c, 'd> Function<'c, 'd> for BooleanFn {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(1)?;
        Ok(&Value::Boolean(args[0].boolean()))
    }
}

struct Not;

impl<'c, 'd> Function<'c, 'd> for Not {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_boolean()?;
        Ok(&Value::Boolean(!arg))
    }
}

struct BooleanLiteral(bool);

impl<'c, 'd> Function<'c, 'd> for BooleanLiteral {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let args = Args(args);
        args.exactly(0)?;
        Ok(&Value::Boolean(self.0))
    }
}

fn true_fn() -> BooleanLiteral {
    BooleanLiteral(true)
}
fn false_fn() -> BooleanLiteral {
    BooleanLiteral(false)
}

struct NumberFn;

impl<'c, 'd> Function<'c, 'd> for NumberFn {
    fn evaluate(
        &self,
        context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.at_most(1)?;
        let arg = args.pop_value_or_context_node(context);
        Ok(&Value::Number(arg.number()))
    }
}

struct Sum;

impl<'c, 'd> Function<'c, 'd> for Sum {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_nodeset()?;
        let r = arg
            .iter()
            .map(|n| str_to_num(&n.string_value()))
            .fold(0.0, |acc, i| acc + i);
        Ok(&Value::Number(r))
    }
}

struct NumberConvert(fn(f64) -> f64);

impl<'c, 'd> Function<'c, 'd> for NumberConvert {
    fn evaluate(
        &self,
        _context: &context::EvaluationCtx,
        args: Vec<&Value>,
    ) -> Result<&Value, Error> {
        let mut args = Args(args);
        args.exactly(1)?;
        let arg = args.pop_number()?;
        Ok(&Value::Number(self.0(arg)))
    }
}

fn floor() -> NumberConvert {
    NumberConvert(f64::floor)
}
fn ceiling() -> NumberConvert {
    NumberConvert(f64::ceil)
}

// https://stackoverflow.com/a/28124775/155423
fn round_ties_to_positive_infinity(x: f64) -> f64 {
    let y = x.floor();
    if x == y {
        x
    } else {
        let z = (2.0 * x - y).floor();
        // Should use copysign
        if x.is_sign_positive() ^ z.is_sign_positive() {
            -z
        } else {
            z
        }
    }
}

fn round() -> NumberConvert {
    NumberConvert(round_ties_to_positive_infinity)
}
