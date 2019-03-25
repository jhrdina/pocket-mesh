let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
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
      {
        name: "value",
        styles:
          ReactDOMRe.Style.make(~fontWeight="bold", ~lineHeight="16px", ()),
      },
    ]
  );

module Styles = {
  let mSize = `px(16);

  open Css;

  let icon = style([width(mSize), height(mSize)]);
};

let component = ReasonReact.statelessComponent("MembersCount");

let make = (~count, ~className="", _children) => {
  ...component,
  render: _self =>
    <UseHook
      hook=useStyles
      render={classes =>
        <div className={[classes##wrapper, className] |> String.concat(" ")}>
          <Icons.Person classes=[Root(classes##icon)] />
          <MaterialUi.Typography
            color=`TextSecondary
            variant=`Caption
            classes=[Root(classes##value)]>
            {count |> string_of_int |> ReasonReact.string}
          </MaterialUi.Typography>
        </div>
      }
    />,
};