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
      styles: ReactDOMRe.Style.make(~marginLeft="-16px", ~flexShrink="0", ()),
    },
  ]);

let make =
    (
      ~signalState,
      ~inGroup,
      ~connectionState,
      ~name,
      ~onClick=_ => (),
      _children,
    ) => {
  ...component,
  render: _self => {
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
            <ListItemText primary={name |> ReasonReact.string} />
          </ListItem>
        }
      />
    );
  },
};