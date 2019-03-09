let component = ReasonReact.statelessComponent("PeerRow");

let useStyles =
  MuiStylesHooks.make([
    {
      name: "checkbox",
      styles:
        ReactDOMRe.Style.make(
          ~marginTop="-12px",
          ~marginBottom="-12px",
          ~marginRight="-16px",
          (),
        ),
    },
    {
      name: "statusIndicator",
      styles: ReactDOMRe.Style.make(~marginLeft="-16px", ()),
    },
  ]);

let make =
    (
      ~signalState,
      ~inGroup,
      ~connectionState,
      ~alias,
      ~onClick=_ => (),
      _children,
    ) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <ListItem onClick button=true>
            <PeerStatusIndicator
              signalState
              inGroup
              connectionState
              className=classes##statusIndicator
            />
            // <Checkbox className=classes##checkbox />
            <ListItemText primary={alias |> ReasonReact.string} />
          </ListItem>
        }
      />
    ),
};