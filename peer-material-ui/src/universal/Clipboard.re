[@bs.val] [@bs.scope ("window", "navigator", "clipboard")]
external writeText: string => Js.Promise.t(unit) = "";