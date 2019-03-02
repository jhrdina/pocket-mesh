// Allows to show checkbox on the left side of the row
let component = ReasonReact.statelessComponent("SelectableRow");

module Styles = {
  open Css;

  let wrapper = style([display(`flex)]);

  let content = style([flex(1)]);
};

let make = (~selected, ~onChange, ~selectable=true, children) => {
  ...component,
  render: _self =>
    <div className=Styles.wrapper>
      {if (selectable) {
         <MaterialUi.Checkbox checked={`Bool(selected)} onChange>
           {"Text" |> ReasonReact.string}
         </MaterialUi.Checkbox>;
       } else {
         ReasonReact.null;
       }}
      <div className=Styles.content> {children |> ReasonReact.array} </div>
    </div>,
};