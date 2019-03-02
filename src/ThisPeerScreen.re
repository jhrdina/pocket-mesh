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
        name: "toolbarTitle",
        styles:
          ReactDOMRe.Style.make(
            ~flexGrow="1",
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.primaryGet
              ),
            (),
          ),
      },
      {
        name: "idBox",
        styles: ReactDOMRe.Style.make(~margin="11px 16px", ()),
      },
      {
        name: "removeAllButton",
        styles:
          ReactDOMRe.Style.make(
            ~margin="11px 16px",
            ~backgroundColor=MaterialUi.Colors.Red.c500,
            ~color="white",
            ~display="block",
            (),
          ),
      },
      {
        name: "removeAllWarning",
        styles: ReactDOMRe.Style.make(~margin="11px 16px", ()),
      },
      {
        name: "exportButton",
        styles: ReactDOMRe.Style.make(~margin="11px 16px", ()),
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
                <Typography
                  variant=`H6 color=`Inherit className={classes##toolbarTitle}>
                  {"This peer" |> ReasonReact.string}
                </Typography>
              </Toolbar>
            </AppBar>
            <SectionTitle text="This peer's ID" />
            <IdBox
              className=classes##idBox
              id="YXNkZmZmc2Rmc2FzZGZzZGZzZnNkZnNkZg=="
            />
            /*<SectionTitle text="Export" />
              <Button
                variant=`Contained
                color=`Primary
                className=classes##exportButton>
                {"Export all local data" |> ReasonReact.string}
              </Button>*/
            <SectionTitle text="Local data removal" />
            <Button
              variant=`Contained
              color=`Primary
              className=classes##removeAllButton>
              {"Remove identity and all data in this peer"
               |> ReasonReact.string}
            </Button>
            <Typography
              variant=`Body2 color=`Error className=classes##removeAllWarning>
              <strong>
                {"Removes everything in the local node: list of friends, list of groups, all local contents of the groups. "
                 |> ReasonReact.string}
              </strong>
              {"New identity will be created for this peer. Depending on your groups settings, data may survive in the other peer's replicas."
               |> ReasonReact.string}
            </Typography>
          </div>
        }
      />
    ),
};