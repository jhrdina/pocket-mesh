external renderFunctionToChildren: 'b => 'a = "%identity";

let hookWrapper =
  Obj.magic(
    (
      props: {
        .
        [@bs.meth] "hook": unit => 'a,
        [@bs.meth] "children": 'a => ReasonReact.reactElement,
      },
    ) => {
    let result = props##hook();
    props##children(result);
  });

let make = (~hook: (. unit) => 'a, ~render: 'a => ReasonReact.reactElement, _) => {
  ReasonReact.wrapJsForReason(
    ~reactClass=hookWrapper, ~props={"hook": hook}, result =>
    render(result)->renderFunctionToChildren
  );
};