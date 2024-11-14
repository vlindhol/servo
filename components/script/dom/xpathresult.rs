/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use dom_struct::dom_struct;
use js::rust::HandleObject;

use crate::dom::bindings::codegen::Bindings::XPathResultBinding::{
    XPathResultConstants, XPathResultMethods,
};
use crate::dom::bindings::error::{Error, Fallible};
use crate::dom::bindings::reflector::{reflect_dom_object_with_proto, Reflector};
use crate::dom::bindings::root::{Dom, DomRoot};
use crate::dom::bindings::str::DOMString;
use crate::dom::node::Node;
use crate::dom::window::Window;
use crate::script_runtime::CanGc;

#[repr(u16)]
#[derive(Clone, Copy, Debug, Eq, JSTraceable, MallocSizeOf, Ord, PartialEq, PartialOrd)]
pub enum XPathResultType {
    Any = XPathResultConstants::ANY_TYPE,
    Number = XPathResultConstants::NUMBER_TYPE,
    String = XPathResultConstants::STRING_TYPE,
    Boolean = XPathResultConstants::BOOLEAN_TYPE,
    UnorderedNodeIterator = XPathResultConstants::UNORDERED_NODE_ITERATOR_TYPE,
    OrderedNodeIterator = XPathResultConstants::ORDERED_NODE_ITERATOR_TYPE,
    UnorderedNodeSnapshot = XPathResultConstants::UNORDERED_NODE_SNAPSHOT_TYPE,
    OrderedNodeSnapshot = XPathResultConstants::ORDERED_NODE_SNAPSHOT_TYPE,
    AnyUnorderedNode = XPathResultConstants::ANY_UNORDERED_NODE_TYPE,
    FirstOrderedNode = XPathResultConstants::FIRST_ORDERED_NODE_TYPE,
}

impl TryFrom<u16> for XPathResultType {
    type Error = ();

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        match value {
            XPathResultConstants::ANY_TYPE => Ok(Self::Any),
            XPathResultConstants::NUMBER_TYPE => Ok(Self::Number),
            XPathResultConstants::STRING_TYPE => Ok(Self::String),
            XPathResultConstants::BOOLEAN_TYPE => Ok(Self::Boolean),
            XPathResultConstants::UNORDERED_NODE_ITERATOR_TYPE => Ok(Self::UnorderedNodeIterator),
            XPathResultConstants::ORDERED_NODE_ITERATOR_TYPE => Ok(Self::OrderedNodeIterator),
            XPathResultConstants::UNORDERED_NODE_SNAPSHOT_TYPE => Ok(Self::UnorderedNodeSnapshot),
            XPathResultConstants::ORDERED_NODE_SNAPSHOT_TYPE => Ok(Self::OrderedNodeSnapshot),
            XPathResultConstants::ANY_UNORDERED_NODE_TYPE => Ok(Self::AnyUnorderedNode),
            XPathResultConstants::FIRST_ORDERED_NODE_TYPE => Ok(Self::FirstOrderedNode),
            _ => Err(()),
        }
    }
}

#[dom_struct]
pub struct XPathResult {
    reflector_: Reflector,
    window: Dom<Window>,
    result_type: XPathResultType,
    invalid_iterator: bool,
}

impl XPathResult {
    fn new_inherited(window: &Window, result_type: XPathResultType) -> XPathResult {
        XPathResult {
            reflector_: Reflector::new(),
            window: Dom::from_ref(window),
            result_type,
            invalid_iterator: false,
        }
    }

    /// `result_type` should be anything but `XPathResultType::Any`. That value is only
    /// used when evaluating an XPath expression and the caller says "you decide which
    /// type this result will have".
    pub fn new(
        window: &Window,
        proto: Option<HandleObject>,
        can_gc: CanGc,
        result_type: XPathResultType,
    ) -> DomRoot<XPathResult> {
        reflect_dom_object_with_proto(
            Box::new(XPathResult::new_inherited(window, result_type)),
            window,
            proto,
            can_gc,
        )
    }
}

impl XPathResultMethods for XPathResult {
    fn ResultType(&self) -> u16 {
        self.result_type as u16
    }

    fn GetNumberValue(&self) -> Fallible<f64> {
        if !matches!(self.result_type, XPathResultType::Number) {
            return Err(Error::Type(
                "Can't get number value for non-number XPathResult".to_string(),
            ));
        }
        todo!()
    }

    fn GetStringValue(&self) -> Fallible<DOMString> {
        if !matches!(self.result_type, XPathResultType::String) {
            return Err(Error::Type(
                "Can't get string value for non-string XPathResult".to_string(),
            ));
        }
        todo!()
    }

    fn GetBooleanValue(&self) -> Fallible<bool> {
        if !matches!(self.result_type, XPathResultType::Boolean) {
            return Err(Error::Type(
                "Can't get boolean value for non-boolean XPathResult".to_string(),
            ));
        }
        todo!()
    }

    /// Should error out if `result_type` isn't node-set compatible or if the DOM
    /// has been mutated since evaluating the XPath expression.
    fn IterateNext(&self) -> Fallible<Option<DomRoot<Node>>> {
        if !matches!(
            self.result_type,
            XPathResultType::OrderedNodeIterator | XPathResultType::UnorderedNodeIterator
        ) {
            return Err(Error::Type(
                "Can't iterate on XPathResult that is not a node-set".to_string(),
            ));
        }
        todo!()
    }

    fn GetInvalidIteratorState(&self) -> Fallible<bool> {
        if self.invalid_iterator
            || matches!(
                self.result_type,
                XPathResultType::OrderedNodeIterator | XPathResultType::UnorderedNodeIterator
            )
        {
            Ok(self.invalid_iterator)
        } else {
            Err(Error::Type(
                "Can't iterate on XPathResult that is not a node-set".to_string(),
            ))
        }
    }

    fn GetSnapshotLength(&self) -> Fallible<u32> {
        if !matches!(
            self.result_type,
            XPathResultType::OrderedNodeSnapshot | XPathResultType::UnorderedNodeSnapshot
        ) {
            return Err(Error::Type(
                "Can't get snapshot length of XPathResult that is not a snapshot".to_string(),
            ));
        }

        todo!()
    }

    fn SnapshotItem(&self, _index: u32) -> Fallible<Option<DomRoot<Node>>> {
        if !matches!(
            self.result_type,
            XPathResultType::OrderedNodeSnapshot | XPathResultType::UnorderedNodeSnapshot
        ) {
            return Err(Error::Type(
                "Can't get snapshot item of XPathResult that is not a snapshot".to_string(),
            ));
        }

        todo!()
    }

    fn GetSingleNodeValue(&self) -> Fallible<Option<DomRoot<Node>>> {
        if !matches!(
            self.result_type,
            XPathResultType::AnyUnorderedNode | XPathResultType::FirstOrderedNode
        ) {
            return Err(Error::Type(
                "Can't get first node on XPathResult that is not of type 'any unordered node' or 'first ordered node'".to_string(),
            ));
        }

        todo!()
    }
}
