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
  render: _self => {
    let peerId =
      PocketMeshPeer.Peer.Id.ofString("YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg==");
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
            <SectionTitle text="Status" />
            <SectionValue text="Online" />
            <SectionTitle text="P2P connection" />
            <ListItem>
              <ListItemText
                primary={"Not established" |> ReasonReact.string}
                secondary={
                  "Connection is not needed because peer is not added in any group."
                  |> ReasonReact.string
                }
              />
            </ListItem>
            <SectionTitle text="Peer ID" />
            <TextField
              className=classes##peerIdField
              inputProps={
                "classes": {
                  "root": classes##peerIdFieldInput,
                },
              }
              value={
                      `String(
                        peerId->Belt.Option.mapWithDefault(
                          "",
                          PocketMeshPeer.Peer.Id.toString,
                        ),
                      )
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
    );
  },
};