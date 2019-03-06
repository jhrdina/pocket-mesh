// TYPES

type state = {toastOpen: bool};

type action =
  | ClickedCopy(string)
  | Copied
  | ToastTimedout;

// UPDATES

let initialState = () => {toastOpen: false};

let reducer = (action, _state) =>
  switch (action) {
  | ClickedCopy(text) =>
    ReasonReact.SideEffects(
      self =>
        Clipboard.writeText(text)
        |> Js.Promise.then_(_ => self.send(Copied) |> Js.Promise.resolve)
        |> ignore,
    )
  | Copied => ReasonReact.Update({toastOpen: true})
  | ToastTimedout => ReasonReact.Update({toastOpen: false})
  };

// VIEW

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

let component = ReasonReact.reducerComponent("IdBox");

let make = (~id, ~className="", _children) => {
  ...component,
  initialState,
  reducer,
  render: self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className={[classes##root, className] |> String.concat(" ")}>
            <div className=classes##id> {id |> ReasonReact.string} </div>
            <Button
              className=classes##copyBtn
              onClick={_ => self.send(ClickedCopy(id))}>
              {"Copy" |> ReasonReact.string}
            </Button>
            <Snackbar
              key="clipboard"
              open_={self.state.toastOpen}
              autoHideDuration={`Int(5000)}
              onClose={(_, _) => self.send(ToastTimedout)}
              message={"ID was copied to clipboard." |> ReasonReact.string}
            />
          </div>
        }
      />
    ),
};