let component = ReasonReact.statelessComponent("PeerRow");

let useStyles =
  MuiStylesHooks.make([
    {name: "root", styles: ReactDOMRe.Style.make(~paddingRight="120px", ())},
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
      styles:
        ReactDOMRe.Style.make(
          ~right="12px",
          ~display="flex",
          ~alignItems="center",
          (),
        ),
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
    {
      name: "membersCount",
      styles: ReactDOMRe.Style.make(~marginRight="8px", ()),
    },
  ]);

let make =
    (
      ~alias,
      ~membersPreview,
      ~membersCount,
      ~onClick=_ => (),
      ~onOpenClick=_ => (),
      _children,
    ) => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <ListItem
            button=true onClick classes=[SecondaryAction(classes##root)]>
            <ListItemText
              primary={alias |> ReasonReact.string}
              secondary={
                String.concat(", ", membersPreview) |> ReasonReact.string
              }
              classes=[Secondary(classes##secondaryText)]
            />
            <ListItemSecondaryAction className=classes##secondaryAction>
              <MembersCount
                count=membersCount
                className=classes##membersCount
              />
              <Button variant=`Outlined color=`Secondary onClick=onOpenClick>
                {"Open" |> ReasonReact.string}
              </Button>
            </ListItemSecondaryAction>
          </ListItem>
        }
      />
    ),
};