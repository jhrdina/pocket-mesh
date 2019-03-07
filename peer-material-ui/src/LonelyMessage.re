let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(~textAlign="center", ~padding="8px 16px", ()),
      },
    ]
  );

let component = ReasonReact.statelessComponent("LonelyMessage");

let make = (~text, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <Typography variant=`Body2 className=classes##root>
            {text |> ReasonReact.string}
          </Typography>
        }
      />
    ),
};