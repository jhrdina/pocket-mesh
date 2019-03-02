// This file is written directly in JS because we need exact control over imports order to ensure that the following module gets imported before everything else:
import "./BootstrapNewMUIStyles";

import { renderToElementWithId } from "reason-react/src/ReactDOMRe";
import { element } from "reason-react/src/ReasonReact";
import { make } from "./App.bs";

renderToElementWithId(element(undefined, undefined, make([])), "app");
