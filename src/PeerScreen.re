let component = ReasonReact.statelessComponent("PeerScreen");

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {name: "root", styles: ReactDOMRe.Style.make()},
      {
        name: "appBar",
        styles:
          ReactDOMRe.Style.make(
            ~backgroundColor=MaterialUi.Colors.Grey.c200,
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
      {
        name: "toolbar",
        styles:
          ReactDOMRe.Style.make(~paddingLeft="0", ~paddingRight="0", ()),
      },
      {
        name: "titleInput",
        styles:
          ReactDOMRe.Style.make(
            ~flex="1",
            ~borderRadius="4px",
            ~padding="0 8px",
            ~backgroundColor=MaterialUi.Colors.Grey.c50,
            ~fontWeight="500",
            ~fontSize="1.125rem",
            (),
          ),
      },
      {
        name: "sectionTitle",
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
      {
        name: "sectionValue",
        styles: ReactDOMRe.Style.make(~padding="11px 16px", ()),
      },
      {
        name: "peerIdField",
        styles:
          ReactDOMRe.Style.make(
            ~margin="11px 16px",
            ~width="calc(100% - 32px)",
            (),
          ),
      },
      {
        name: "peerIdFieldInput",
        styles:
          ReactDOMRe.Style.make(
            ~fontSize="14px",
            ~fontFamily="monospace",
            ~wordBreak="break-all",
            ~lineHeight="20px",
            (),
          ),
      },
      {
        name: "copyButtonBox",
        styles:
          ReactDOMRe.Style.make(
            ~margin="-7px 16px 11px 16px",
            ~textAlign="right",
            (),
          ),
      },
    ]
  );

let make = _children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##root>
            <AppBar position=`Static className={classes##appBar}>
              <Toolbar variant=`Dense className=classes##toolbar>
                <IconButton color=`Inherit> <Icons.ArrowBack /> </IconButton>
                <InputBase
                  placeholder="Peer alias"
                  className=classes##titleInput
                />
                <div>
                  <IconButton color=`Inherit> <Icons.Delete /> </IconButton>
                </div>
              </Toolbar>
            </AppBar>
            <Typography variant=`Subtitle2 className=classes##sectionTitle>
              {"Status" |> ReasonReact.string}
            </Typography>
            <Typography variant=`Body1 className=classes##sectionValue>
              {"Online" |> ReasonReact.string}
            </Typography>
            <Typography variant=`Subtitle2 className=classes##sectionTitle>
              {"P2P connection" |> ReasonReact.string}
            </Typography>
            <ListItem>
              <ListItemText
                primary={"Not established" |> ReasonReact.string}
                secondary={
                  "Connection is not needed because peer is not added in any group."
                  |> ReasonReact.string
                }
              />
            </ListItem>
            <Typography variant=`Subtitle2 className=classes##sectionTitle>
              {"Peer ID" |> ReasonReact.string}
            </Typography>
            <TextField
              className=classes##peerIdField
              inputProps={
                "classes": {
                  "root": classes##peerIdFieldInput,
                },
              }
              variant=`Outlined
              multiline=true
              fullWidth=true
              placeholder="Insert peer's ID here..."
            />
            <div className=classes##copyButtonBox>
              <Button> {"Copy to clipboard" |> ReasonReact.string} </Button>
            </div>
          </div>
        }
      />
    ),
};