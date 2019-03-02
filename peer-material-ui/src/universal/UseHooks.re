external renderFunctionToChildren: 'b => 'a = "%identity";

let hookWrapper =
  Obj.magic(
    (
      props: {
        .
        "hooks": ((. unit) => 'a, (. unit) => 'b),
        [@bs.meth] "children": (('a, 'b)) => ReasonReact.reactElement,
      },
    ) => {
    let (aHook, bHook) = props##hooks;
    let a = aHook(.);
    let b = bHook(.);
    props##children((a, b));
  });

let make =
    (
      ~hooks: ((. unit) => 'a, (. unit) => 'b),
      ~render: (('a, 'b)) => ReasonReact.reactElement,
      _,
    ) => {
  ReasonReact.wrapJsForReason(
    ~reactClass=hookWrapper, ~props={"hooks": hooks}, results =>
    render(results)->renderFunctionToChildren
  );
};