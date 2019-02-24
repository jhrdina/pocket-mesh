let muiOverrides: MaterialUi.Theme.t => list(MaterialUi.WithStyles.style) =
  theme => [
    {name: "wrapper", styles: ReactDOMRe.Style.make(~display="flex", ())},
    {
      name: "icon",
      styles:
        ReactDOMRe.Style.make(
          ~width="16px",
          ~height="16px",
          ~color=
            MaterialUi.Theme.(
              theme
              |> Theme.paletteGet
              |> Palette.textGet
              |> TypeText.secondaryGet
            ),
          (),
        ),
    },
    {name: "value", styles: ReactDOMRe.Style.make(~fontWeight="bold", ())},
  ];

module Styles = {
  let mSize = `px(16);

  open Css;

  let icon = style([width(mSize), height(mSize)]);
};

let component = ReasonReact.statelessComponent("MembersCount");

let make = (~count, _children) => {
  ...component,
  render: _self =>
    <MaterialUi.WithStyles
      classesWithTheme=muiOverrides
      render={classes =>
        <div className=classes##wrapper>
          <Icons.Person classes=[Root(classes##icon)] />
          <MaterialUi.Typography
            variant=`Caption classes=[Root(classes##value)]>
            {count |> string_of_int |> ReasonReact.string}
          </MaterialUi.Typography>
        </div>
      }
    />,
};