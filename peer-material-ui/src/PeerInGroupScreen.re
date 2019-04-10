open Infix;

module RightArrow = {
  let component = ReasonReact.statelessComponent("RightArrow");
  let make = (~className="", _ch) => {
    ...component,
    render: _self =>
      <MaterialUi.SvgIcon className viewBox="0 0 20 20">
        <path
          d="M 6.4140622,0 3.5859372,2.828125 10.757813,10 3.5859372,17.171875 6.4140622,20 13.585938,12.828125 16.414063,10 13.585938,7.171875 Z"
        />
      </MaterialUi.SvgIcon>,
  };
};

module LeftArrow = {
  let component = ReasonReact.statelessComponent("LeftArrow");
  let make = (~className="", _ch) => {
    ...component,
    render: _self =>
      <MaterialUi.SvgIcon className viewBox="0 0 20 20">
        <path
          d="M 13.585994,8.6944119e-6 16.414117,2.828114 9.242247,9.9999899 16.414117,17.171866 13.585994,20.000009 6.4141229,12.828133 3.586,9.9999899 6.4141229,7.1718845 Z"
        />
      </MaterialUi.SvgIcon>,
  };
};

module Cross = {
  let component = ReasonReact.statelessComponent("Cross");
  let make = (~className="", _ch) => {
    ...component,
    render: _self =>
      <MaterialUi.SvgIcon className viewBox="0 0 20 20">
        <path
          d="M 2.828125,0 0,2.82811 7.1718747,9.99999 0,17.17187 2.828125,20.00001 10,12.82813 17.171875,20.00001 20,17.17187 12.828125,9.99999 20,2.82811 17.171875,0 10,7.17188 Z"
        />
      </MaterialUi.SvgIcon>,
  };
};

type arrowDirection =
  | Left
  | Right;

let signalLineColor = MaterialUi.Colors.Grey.c400;

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
            ~backgroundColor="#ffffff",
            ~boxShadow="none",
            ~color=
              MaterialUi.Theme.(
                theme
                |> Theme.paletteGet
                |> Palette.textGet
                |> TypeText.secondaryGet
              ),
            ~borderBottom="1px dashed #cccccc",
            (),
          ),
      },
      {
        name: "toolbar",
        styles:
          ReactDOMRe.Style.make(~paddingLeft="0", ~paddingRight="0", ()),
      },
      {
        name: "toolbarTitleBox",
        styles: ReactDOMRe.Style.make(~flex="1", ~overflow="hidden", ()),
      },
      {
        name: "toolbarPeerAlias",
        styles:
          ReactDOMRe.Style.make(~lineHeight="1", ~fontSize="1.125rem", ()),
      },
      {
        name: "toolbarSubtitle",
        styles:
          ReactDOMRe.Style.make(
            ~lineHeight="1.4",
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
        name: "removeBtn",
        styles: ReactDOMRe.Style.make(~color=MaterialUi.Colors.Red.c500, ()),
      },
      {
        name: "diaHorizLinesPair",
        styles:
          ReactDOMRe.Style.make(
            ~margin="0 8px",
            ~padding="4px 0",
            ~borderTop="4px solid " ++ signalLineColor,
            ~borderBottom="4px solid " ++ signalLineColor,
            ~position="relative",
            (),
          ),
      },
      {
        name: "diaChannelLabel",
        styles: ReactDOMRe.Style.make(~textAlign="center", ()),
      },
      {
        name: "diaArrow",
        styles:
          ReactDOMRe.Style.make(
            ~position="absolute",
            ~transform="translate(-50%,-50%)",
            ~color=signalLineColor,
            (),
          ),
      },
      {
        name: "diaArrowTopLeft",
        styles: ReactDOMRe.Style.make(~top="-2px", ~left="24px", ()),
      },
      {
        name: "diaArrowTopRight",
        styles: ReactDOMRe.Style.make(~top="-2px", ~right="2px", ()),
      },
      {
        name: "diaSideLine",
        styles:
          ReactDOMRe.Style.make(
            ~top="0",
            ~width="4px",
            ~bottom="0",
            ~backgroundColor=MaterialUi.Colors.Grey.c300,
            ~position="absolute",
            (),
          ),
      },
      {
        name: "diaArrowBottomLeft",
        styles:
          ReactDOMRe.Style.make(
            ~bottom="-34px",
            ~left="24px",
            ~backgroundColor="rgba(255,255,255,0.7)",
            ~borderRadius="50%",
            ~minWidth="10px",
            ~padding="4px",
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
        name: "diaArrowBottomRight",
        styles: ReactDOMRe.Style.make(~bottom="-26px", ~right="2px", ()),
      },
      {
        name: "diaNotLastChannel",
        styles: ReactDOMRe.Style.make(~marginBottom="52px", ()),
      },
    ]
  );

let renderArrowState = (~className="", ~available, ~direction, ()) => {
  switch (available, direction) {
  | (Some(true), Left) => <LeftArrow className />
  | (Some(true), Right) => <RightArrow className />
  | (Some(false), _) => <Cross className />
  | (None, _) => <div> {"?" |> ReasonReact.string} </div>
  };
};

let renderChannelDiagram =
    (
      ~label,
      ~peerAcceptState,
      ~peerSendState,
      ~thisPeerAcceptState,
      ~onClick=() => (),
      ~classes,
      ~className="",
      (),
    ) => {
  <div
    className={[classes##diaHorizLinesPair, className] |> String.concat(" ")}>
    {renderArrowState(
       ~available=Some(true),
       ~className=classes##diaArrow ++ " " ++ classes##diaArrowTopLeft,
       ~direction=Right,
       (),
     )}
    {renderArrowState(
       ~available=peerAcceptState,
       ~className=classes##diaArrow ++ " " ++ classes##diaArrowTopRight,
       ~direction=Right,
       (),
     )}
    <MaterialUi.Button
      variant=`Contained
      size=`Small
      color=`Default
      className={classes##diaArrow ++ " " ++ classes##diaArrowBottomLeft}
      onClick={_ => onClick()}>
      {renderArrowState(
         ~available=Some(thisPeerAcceptState),
         ~direction=Left,
         (),
       )}
    </MaterialUi.Button>
    {renderArrowState(
       ~available=peerSendState,
       ~className=classes##diaArrow ++ " " ++ classes##diaArrowBottomRight,
       ~direction=Left,
       (),
     )}
    <MaterialUi.Typography variant=`Body2 className=classes##diaChannelLabel>
      {label |> ReasonReact.string}
    </MaterialUi.Typography>
  </div>;
};

let component = ReasonReact.statelessComponent("PeerInGroupScreen");

let make =
    (~dbState, ~groupId, ~peerInGroup, ~pushMsg, ~className="", _children) => {
  ...component,
  render: _self => {
    let peerId = peerInGroup |> PM.PeerInGroup.id;
    let permissions = peerInGroup |> PM.PeerInGroup.permissions;
    let (acceptsContent, acceptsMembers) =
      switch (permissions) {
      | ReadContentAndMembers => (false, false)
      | WriteContent(ReadMembers) => (true, false)
      | WriteContent(WriteMembers) => (true, true)
      };

    let peerAlias = peerId |> GuiUtils.getPeerVisibleName(~dbState);
    let peerAliasShort =
      peerId |> GuiUtils.getPeerVisibleName(~idMaxChars=6, ~dbState);

    let groupAlias =
      dbState
      |> PM.DbState.groups
      |> PM.PeersGroups.findOpt(groupId)
      |?>> GuiUtils.getPeerGroupVisibleName
      |? (groupId |> PM.PeersGroup.Id.toString);
    MaterialUi.(
      <UseHook
        hook=useStyles
        render={classes =>
          <div
            className={[classes##wrapper, className] |> String.concat(" ")}>
            <AppBar position=`Static className={classes##appBar}>
              <Toolbar variant=`Dense className=classes##toolbar>
                <IconButton
                  color=`Inherit
                  className={classes##toolbarLeftBtn}
                  onClick={_ => pushMsg(Route.ChangeRoute(Group(groupId)))}>
                  <Icons.ArrowBack />
                </IconButton>
                <div className={classes##toolbarTitleBox}>
                  <Typography
                    noWrap=true
                    variant=`H6
                    className={classes##toolbarPeerAlias}>
                    {peerAlias |> ReasonReact.string}
                  </Typography>
                  <Typography
                    noWrap=true
                    variant=`Body2
                    className={classes##toolbarSubtitle}>
                    {"in group "
                     ++ GuiUtils.quote(groupAlias)
                     |> ReasonReact.string}
                  </Typography>
                </div>
              </Toolbar>
            </AppBar>
            // <div className=classes##toolbarRightBlock>
            //   <IconButton color=`Inherit> <Icons.MoreVert /> </IconButton>
            // </div>
            <div
              style={ReactDOMRe.Style.make(~margin="12px 16px 0 16px", ())}>
              <div
                style={ReactDOMRe.Style.make(
                  ~display="flex",
                  ~justifyContent="space-between",
                  ~alignItems="flex-end",
                  (),
                )}>
                <Typography
                  variant=`Subtitle2 className={classes##toolbarSubtitle}>
                  {"This peer" |> ReasonReact.string}
                </Typography>
                <Typography
                  variant=`Subtitle2
                  className={classes##toolbarSubtitle}
                  style={ReactDOMRe.Style.make(~textAlign="right", ())}>
                  {peerAliasShort |> ReasonReact.string}
                </Typography>
              </div>
              <div
                style={ReactDOMRe.Style.make(
                  ~position="relative",
                  ~padding="16px 8px",
                  ~margin="4px 0",
                  (),
                )}>
                <div
                  className=classes##diaSideLine
                  style={ReactDOMRe.Style.make(~left="8px", ())}
                />
                <div
                  className=classes##diaSideLine
                  style={ReactDOMRe.Style.make(~right="8px", ())}
                />
                {renderChannelDiagram(
                   ~label="Content",
                   ~peerAcceptState=Some(true),
                   ~peerSendState=Some(true),
                   ~thisPeerAcceptState=acceptsContent,
                   ~classes,
                   ~className=classes##diaNotLastChannel,
                   ~onClick=
                     () =>
                       pushMsg(
                         Msg.ReqP2PMsg(
                           PM.Msg.updatePeerPermissions(
                             peerId,
                             groupId,
                             switch (permissions) {
                             | ReadContentAndMembers =>
                               WriteContent(ReadMembers)
                             | WriteContent(_) => ReadContentAndMembers
                             },
                           ),
                         ),
                       ),
                   (),
                 )}
                {renderChannelDiagram(
                   ~label="Members list",
                   ~peerAcceptState=Some(true),
                   ~peerSendState=Some(true),
                   ~thisPeerAcceptState=acceptsMembers,
                   ~classes,
                   ~onClick=
                     () => {
                       Js.log("clicked");
                       pushMsg(
                         Msg.ReqP2PMsg(
                           PM.Msg.updatePeerPermissions(
                             peerId,
                             groupId,
                             switch (permissions) {
                             | ReadContentAndMembers
                             | WriteContent(ReadMembers) =>
                               WriteContent(WriteMembers)
                             | WriteContent(WriteMembers) =>
                               WriteContent(ReadMembers)
                             },
                           ),
                         ),
                       );
                     },
                   (),
                 )}
              </div>
            </div>
            <Button
              fullWidth=true
              className=classes##removeBtn
              onClick={_ => {
                pushMsg(
                  Msg.ReqP2PMsg(PM.Msg.removePeerFromGroup(peerId, groupId)),
                );
                pushMsg(Route.ChangeRoute(Group(groupId)));
              }}>
              {"Remove from group" |> ReasonReact.string}
            </Button>
          </div>
        }
      />
    );
  },
};