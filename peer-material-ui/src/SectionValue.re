let component = ReasonReact.statelessComponent("SectionValue");

let useStyles =
  MuiStylesHooks.makeWithTheme(_theme =>
    [
      {name: "root", styles: ReactDOMRe.Style.make(~padding="11px 16px", ())},
    ]
  );

let make = (~text, ~className="", _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <Typography
            variant=`Body1
            className={[classes##root, className] |> String.concat(" ")}>
            {text |> ReasonReact.string}
          </Typography>
        }
      />
    ),
};