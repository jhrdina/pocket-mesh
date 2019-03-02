let component = ReasonReact.statelessComponent("SectionTitle");

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(
            ~padding="16px 16px 0 16px",
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
            variant=`Subtitle2
            className={[classes##root, className] |> String.concat(" ")}>
            {text |> ReasonReact.string}
          </Typography>
        }
      />
    ),
};