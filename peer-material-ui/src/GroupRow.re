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
    {
      name: "secondaryAction",
      styles: ReactDOMRe.Style.make(~right="12px", ()),
    },
    {
      name: "secondaryText",
      styles:
        ReactDOMRe.Style.make(
          ~textOverflow="ellipsis",
          ~whiteSpace="nowrap",
          ~overflow="hidden",
          (),
        ),
    },
  ];

module Styles = {
  open Css;
};

let make = (~alias, ~membersPreview, ~membersCount, _children) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <WithStyles
        classesWithTheme=muiStyles
        render={classes =>
          <ListItem button=true>
            // <Checkbox className=classes##checkbox />

              <ListItemText
                primary={alias |> ReasonReact.string}
                secondary={
                  String.concat(", ", membersPreview) |> ReasonReact.string
                }
                classes=[Secondary(classes##secondaryText)]
              />
              <ListItemSecondaryAction className=classes##secondaryAction>
                <MembersCount count=membersCount />
              </ListItemSecondaryAction>
            </ListItem>
        }
      />
    ),
};