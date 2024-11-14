/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use dom_struct::dom_struct;
use js::rust::HandleObject;

use crate::dom::bindings::codegen::Bindings::XPathExpressionBinding::XPathExpressionMethods;
use crate::dom::bindings::error::{Error, Fallible};
use crate::dom::bindings::reflector::DomObject;
use crate::dom::bindings::reflector::{reflect_dom_object_with_proto, Reflector};
use crate::dom::bindings::root::{Dom, DomRoot};
use crate::dom::bindings::str::DOMString;
use crate::dom::node::Node;
use crate::dom::window::Window;
use crate::dom::xpathresult::XPathResultType;
use crate::script_runtime::CanGc;

use super::types::XPathResult;

#[dom_struct]
pub struct XPathExpression {
    reflector_: Reflector,
    window: Dom<Window>,
}

impl XPathExpression {
    fn new_inherited(window: &Window, _expression: DOMString) -> XPathExpression {
        XPathExpression {
            reflector_: Reflector::new(),
            window: Dom::from_ref(window),
        }
    }

    pub fn new(
        window: &Window,
        proto: Option<HandleObject>,
        can_gc: CanGc,
        expression: DOMString,
    ) -> DomRoot<XPathExpression> {
        reflect_dom_object_with_proto(
            Box::new(XPathExpression::new_inherited(window, expression)),
            window,
            proto,
            can_gc,
        )
    }
}

impl XPathExpressionMethods for XPathExpression {
    fn Evaluate(
        &self,
        _context_node: &Node,
        result_type_num: u16,
        _result: Option<&super::types::XPathResult>,
    ) -> Fallible<DomRoot<super::types::XPathResult>> {
        let _result_type = XPathResultType::try_from(result_type_num)
            .map_err(|()| Error::Type("Invalid XPath result type".to_string()))?;

        let global = self.global();
        let window = global.as_window();
        let result = XPathResult::new(window, None, CanGc::note(), _result_type);

        // TODO: support putting results into mutable `_result` as per the spec
        // TODO: actually construct a result
        Ok(result)
    }
}
