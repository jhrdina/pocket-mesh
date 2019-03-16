open BlackTea;
open Infix;

// TYPES

type model = {peerSearchDialogOpen: bool};
type Msg.t +=
  | ClickedAddPeer
  | ClosedPeerSearchDialog(PeerSearchScreen.closeResult);

// UPDATE

let init = () => ({peerSearchDialogOpen: false}, Cmd.none);

let update = (~groupId, msg, model) => {
  switch (msg) {
  | ClosedPeerSearchDialog(Cancel) => (
      {peerSearchDialogOpen: false},
      Cmd.none,
    )
  | ClosedPeerSearchDialog(Ok(peerId)) => (
      {peerSearchDialogOpen: false},
      Cmd.batch([
        Cmd.msg(
          Msg.ReqP2PMsg(
            PM.Msg.addPeerToGroup(peerId, groupId, ReadContentAndMembers),
          ),
        ),
      ]),
    )
  | ClickedAddPeer => ({peerSearchDialogOpen: true}, Cmd.none)
  | _ => (model, Cmd.none)
  };
};

let useStyles =
  MuiStylesHooks.makeWithTheme(theme =>
    [
      {
        name: "wrapper",
        styles: ReactDOMRe.Style.make(~position="relative", ()),
      },
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
        name: "permissionsText",
        styles:
          ReactDOMRe.Style.make(
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            ~fontSize="16px",
            (),
          ),
      },
      {
        name: "memberListItem",
        styles: ReactDOMRe.Style.make(~paddingRight="76px", ()),
      },
      {
        name: "secondaryAction",
        styles: ReactDOMRe.Style.make(~right="16px", ()),
      },
      {
        name: "fab",
        styles:
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~right="16px",
            ~bottom="16px",
            ~zIndex="50",
            (),
          ),
      },
    ]
  );

let component = ReasonReact.statelessComponent("GroupScreen");

let make = (~groupId, ~dbState, ~model, ~pushMsg, _children) => {
  ...component,

  render: _self => {
    let groupAliasStr =
      dbState
      |> PM.DbState.groups
      |> PM.PeersGroups.findOpt(groupId)
      |?>> PM.PeersGroup.alias
      |? "";

    let isEmpty = true;
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div className=classes##wrapper>
            <AppBar position=`Static className={classes##appBar}>

                <Toolbar variant=`Dense className=classes##toolbar>
                  <IconButton
                    color=`Inherit
                    className={classes##toolbarLeftBtn}
                    onClick={_ => pushMsg(Route.ChangeRoute(Main(Groups)))}>
                    <Icons.ArrowBack />
                  </IconButton>
                  <InputBase
                    placeholder="Group name"
                    className=classes##titleInput
                    value={`String(groupAliasStr)}
                    onChange={e =>
                      pushMsg(
                        Msg.ReqP2PMsg(
                          PM.Msg.updateGroupAlias(
                            groupId,
                            e->ReactEvent.Form.target##value,
                          ),
                        ),
                      )
                    }
                  />
                  <div>
                    <IconButton
                      color=`Inherit
                      onClick={_ => {
                        pushMsg(Msg.ReqP2PMsg(PM.Msg.removeGroup(groupId)));
                        pushMsg(Route.ChangeRoute(Main(Groups)));
                      }}>
                      <Icons.Delete />
                    </IconButton>
                  </div>
                </Toolbar>
              </AppBar>
              // <IconButton color=`Inherit> <Icons.MoreVert /> </IconButton>
            <SectionTitle text="Group ID" />
            <IdBox id={groupId |> PM.PeersGroup.Id.toString} />
            <SectionTitle text="Members" />
            <List>
              {PM.(
                 dbState
                 |> DbState.groups
                 |> PeersGroups.findOpt(groupId)
                 |?>> PeersGroup.foldPeersInGroup(
                        (arr, peerInGroup) => {
                          let peerId = peerInGroup |> PeerInGroup.id;
                          let peerIdStr = peerId |> Peer.Id.toString;
                          let displayedName =
                            dbState
                            |> DbState.peers
                            |> Peers.findOpt(peerId)
                            |?> (
                              peer =>
                                peer |> Peer.alias != "" ?
                                  Some(peer |> Peer.alias) : None
                            )
                            |? peerIdStr;

                          let permissionsText =
                            switch (peerInGroup |> PeerInGroup.permissions) {
                            | ReadContentAndMembers => "R / R"
                            | WriteContent(ReadMembers) => "RW / R"
                            | WriteContent(WriteMembers) => "RW / RW"
                            };

                          let listItem =
                            <ListItem
                              button=true
                              key=peerIdStr
                              classes=[
                                SecondaryAction(classes##memberListItem),
                              ]
                              onClick={_ =>
                                pushMsg(
                                  Route.ChangeRoute(
                                    PeerInGroup(peerId, groupId),
                                  ),
                                )
                              }>
                              <ListItemText
                                primary={displayedName |> ReasonReact.string}
                                secondary={peerIdStr |> ReasonReact.string}
                                primaryTypographyProps={"noWrap": true}
                                secondaryTypographyProps={"noWrap": true}
                              />
                              <ListItemSecondaryAction
                                className=classes##secondaryAction>
                                <Typography
                                  variant=`Button
                                  className=classes##permissionsText>
                                  {permissionsText |> ReasonReact.string}
                                </Typography>
                              </ListItemSecondaryAction>
                            </ListItem>;
                          Js.Array.concat(arr, [|listItem|]);
                        },
                        [||],
                      )
                 |? [||]
               )
               |> GuiUtils.elementArrayWithDefaultMsg(
                    "There are no peers added in this group.",
                  )}
            </List>
            <Fab
              color=`Secondary
              className=classes##fab
              onClick={_ => pushMsg(ClickedAddPeer)}>
              <Icons.PersonAdd />
            </Fab>
            <PeerSearchScreen
              dbState
              open_={model.peerSearchDialogOpen}
              onClose={res => pushMsg(ClosedPeerSearchDialog(res))}
            />
          </div>
        }
      />
    );
  },
};