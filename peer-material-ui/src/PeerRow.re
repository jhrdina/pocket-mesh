let component = ReasonReact.statelessComponent("PeerRow");

let muiStyles: MaterialUi.Theme.t => list(MaterialUi.WithStyles.style) =
  _theme => [
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
  ];

module Styles = {
  open Css;
};

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
      <WithStyles
        classesWithTheme=muiStyles
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