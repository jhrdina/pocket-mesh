let component = ReasonReact.statelessComponent("IdBox");

let idBoxBorder = "1px dashed #ccc";

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "root",
        styles:
          ReactDOMRe.Style.make(
            ~margin="0 16px",
            ~display="flex",
            ~border=idBoxBorder,
            ~borderRadius="4px",
            (),
          ),
      },
      {
        name: "id",
        styles:
          ReactDOMRe.Style.make(
            ~flex="1",
            ~fontSize="14px",
            ~fontFamily="monospace",
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            ~padding="8px",
            ~borderRight=idBoxBorder,
            ~wordBreak="break-all",
            ~lineHeight="20px",
            (),
          ),
      },
      {
        name: "copyBtn",
        styles: ReactDOMRe.Style.make(~borderRadius="0 4px 4px 0", ()),
      },
    ]
  );

let make = (~id, ~className="", _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className={[classes##root, className] |> String.concat(" ")}>
            <div className=classes##id> {id |> ReasonReact.string} </div>
            <Button className=classes##copyBtn>
              {"Copy" |> ReasonReact.string}
            </Button>
          </div>
        }
      />
    ),
};