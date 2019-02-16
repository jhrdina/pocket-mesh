open BlackTea;
open Json.Infix;
module PM = PublicInterface;

/* Store.create() |> ignore; */

/* TYPES */

type model = {
  p2p: PM.State.t,
  counter: int,
  // itemsGeneratorGroup: option(PM.PeersGroup.Id.t),
  itemsGenerator: InputEmulator.t,
};

[@bs.deriving accessors]
type msg =
  | PMMsg(PM.Msg.t)
  // | StartItemsGenerator(PM.PeersGroup.Id.t)
  | StopItemsGenerator
  | ItemGenerated(string)
  // | ClearContent(PM.PeersGroup.Id.t)
  | Increment;

/* STUPID HTML RENDERER */

let div = "div";
let btn = "button";
let h1 = "h1";
let h2 = "h2";
let p = "p";
let tr = "tr";
let td = "td";
let table = "table";
let span = "span";

let style = "style";
let cls = "class";
let placeholder = "placeholder";

let hace = (tagName, attrs, id, evtHandlers, children) => {
  let attrs = id != "" ? attrs |> List.append([("id", id)]) : attrs;
  let attrsStr =
    attrs
    |> List.map(((key, value)) => key ++ "=\"" ++ value ++ "\"")
    |> String.concat(" ")
    |> (str => str == "" ? "" : " " ++ str);
  let (childrenStr, events) =
    children
    |> List.fold_left(
         ((childrenStr, events), (htmlStr, evts)) =>
           (childrenStr ++ htmlStr, List.append(events, evts)),
         ("", []),
       );
  let thisElEvents =
    evtHandlers |> List.rev_map(((evtType, cb)) => (id, evtType, cb));

  (
    "<" ++ tagName ++ attrsStr ++ ">" ++ childrenStr ++ "</" ++ tagName ++ ">",
    List.append(thisElEvents, events),
  );
};

let hac = (tagName, attrs, children) =>
  hace(tagName, attrs, "", [], children);

let h = tagName => hac(tagName, [], []);
let ha = (tagName, attrs) => hac(tagName, attrs, []);
let hc = (tagName, children) => hac(tagName, [], children);
let txt = text => (text, []);
let empty = ("", []);

let renderHtml: string => unit = [%bs.raw
  str => "{
    document.getElementById('root').innerHTML = str;
  }"
];

let addEvent: (string, string, unit => unit) => unit = [%bs.raw
  (id, evtType, f) => "{
    const el = document.getElementById(id);
    if (el) {
      el.addEventListener(evtType, f)
    }
  }"
];

let removeEvent: (string, string, unit => unit) => unit = [%bs.raw
  (id, evtType, f) => "{
    const el = document.getElementById(id);
    if (el) {
      el.removeEventListener(evtType, f)
    }
  }"
];

/* HELPERS */
[@bs.set_index]
external makeGlobal: (Webapi.Dom.Window.t, string, 'a) => unit = "";
let makeGlobal = (name, value) => makeGlobal(Webapi.Dom.window, name, value);

let foldContentItems = (f, acc, crdt) =>
  PM.Crdt.(
    crdt
    |> root
    |> Json.Map.get("items")
    |?> Json.List.ofJson
    |?>> Json.List.foldLeft(
           (acc, item) =>
             switch (item |> Json.asString) {
             | Some(strItem) => f(acc, strItem)
             | None => acc
             },
           acc,
         )
  );

let getGroupContentOpt = (groupId, model) =>
  (
    switch (model.p2p |> PM.State.classify) {
    | PM.State.HasIdentity(dbState, _) => Some(dbState)
    | _ => None
    }
  )
  |?>> PM.DbState.groups
  |?> PM.PeersGroups.findOpt(groupId)
  |?>> PM.PeersGroup.content;

let removeAllGroupContentItems = content =>
  content
  |> PM.Crdt.change("Remove all items", root =>
       PM.Crdt.Json.(root |> Map.add("items", List.create() |> List.toJson))
     );

let getThisPeerIdStart = model =>
  switch (model.p2p |> PM.State.classify) {
  | PM.State.HasIdentity(dbState, _) =>
    (dbState |> PM.DbState.thisPeer |> PM.ThisPeer.id |> PM.Peer.Id.toString)
    ->String.sub(0, 5)
  | _ => "NO_ID"
  };

/* VIEWS */

let vSection = (title, children) =>
  hac(
    div,
    [(cls, "section")],
    [hac(h2, [(cls, "section__title")], [title |> txt]), ...children],
  );

let vKeyVal = (key, value) =>
  hac(
    div,
    [(cls, "valbox")],
    [
      hac(div, [(cls, "valbox__bold")], [txt(key ++ ":")]),
      hac(div, [], [txt(value)]),
    ],
  );

let viewPeer = peer => {
  let alias =
    (
      peer |> PM.Peer.alias == "" ?
        {js|– no alias –|js} : peer |> PM.Peer.alias
    )
    |> txt;
  // let connectionState =
  //   (
  //     switch (peer |> PM.Peer.connectionState) {
  //     | NotInGroup(Online) => "not in group, online"
  //     | NotInGroup(Offline) => "not in group, offline"
  //     | InGroupWaitingForOnlineSignal => "in group, offline"
  //     | InGroupOnlineInitiatingConnection(_) => "in group, online, initiating connection..."
  //     | OnlineAcceptingConnection(true) => "in group, online, accepting connection..."
  //     | OnlineAcceptingConnection(false) => "not in group, online, accepting connection..."
  //     | InGroupOnlineFailedRetryingAt(intervalSec, _, _msg) =>
  //       "in group, online, connection failed, retrying in "
  //       ++ (intervalSec |> string_of_int)
  //       ++ "s..."
  //     | Connected(true, Online) => "in group, online, CONNECTED"
  //     | Connected(true, Offline) => "in group, offline, CONNECTED"
  //     | Connected(false, Online) => "not in group, online, CONNECTED"
  //     | Connected(false, Offline) => "not in group, offline, CONNECTED"
  //     }
  //   )
  //   |> txt;
  let id = peer |> PM.Peer.id |> PM.Peer.Id.toString;

  hac(
    div,
    [(cls, "peer")],
    [
      hac(
        div,
        [(cls, "peer__line")],
        [
          hac(div, [(cls, "peer__alias")], [alias]),
          // hac(div, [(cls, "peer__connection-state")], [connectionState]),
          hace(
            btn,
            [],
            "removePeerBtn_" ++ id,
            [("click", PMMsg(PM.Msg.removePeer(peer |> PM.Peer.id)))],
            [txt("x")],
          ),
        ],
      ),
      hac(
        div,
        [(cls, "peer__line")],
        [hac(div, [(cls, "peer__id")], [id |> txt])],
      ),
    ],
  );
};

let viewPeers = peers =>
  vSection(
    "Peers",
    peers
    |> PM.Peers.fold((acc, peer) => [viewPeer(peer), ...acc], [])
    |> List.append([
         hace("input", [(placeholder, "ID")], "addPeerIdInput", [], []),
         hace(btn, [], "addPeerIdBtn", [], [txt("Add")]),
       ]),
  );

let viewPeerInGroup = (group, peerInGroup) => {
  let groupId = group |> PM.PeersGroup.id |> PM.PeersGroup.Id.toString;
  let peerId = peerInGroup |> PM.PeerInGroup.id |> PM.Peer.Id.toString;
  let permsStr =
    switch (peerInGroup |> PM.PeerInGroup.permissions) {
    | ReadContentAndMembers => "C: R, M: R"
    | WriteContent(ReadMembers) => "C: RW, M: R"
    | WriteContent(WriteMembers) => "C: RW, M: RW"
    };
  hac(
    div,
    [(cls, "valbox")],
    [
      hac(
        div,
        [(cls, "valbox__row")],
        [
          hac(div, [], [txt(permsStr)]),
          hace(
            btn,
            [],
            "removePeerFromGroupBtn_" ++ peerId ++ "_" ++ groupId,
            [
              (
                "click",
                PMMsg(
                  PM.Msg.removePeerFromGroup(
                    peerInGroup |> PM.PeerInGroup.id,
                    group |> PM.PeersGroup.id,
                  ),
                ),
              ),
            ],
            [txt("x")],
          ),
        ],
      ),
      hac(
        div,
        [(cls, "valbox__row")],
        [hac(div, [(cls, "valbox__small valbox__grey")], [txt(peerId)])],
      ),
    ],
  );
};

let vSeparator = ha("hr", [(cls, "separator")]);

let viewContentItem = text =>
  hac(div, [(cls, "valbox__small")], [txt(text)]);

let viewGroup = group => {
  let groupId = group |> PM.PeersGroup.id |> PM.PeersGroup.Id.toString;
  hac(
    div,
    [],
    [
      vSeparator,
      vKeyVal("ID", groupId),
      hc(
        div,
        [
          hace(
            btn,
            [],
            "removeGroupBtn_" ++ groupId,
            [
              (
                "click",
                PMMsg(PM.Msg.removeGroup(group |> PM.PeersGroup.id)),
              ),
            ],
            [txt("Remove group")],
          ),
          // hace(
          //   btn,
          //   [],
          //   "startItemsGeneratorBtn_" ++ groupId,
          //   [("click", StartItemsGenerator(group |> PM.PeersGroup.id))],
          //   [txt("Start Gen.")],
          // ),
          // hace(
          //   btn,
          //   [],
          //   "stopItemsGeneratorBtn_" ++ groupId,
          //   [("click", StopItemsGenerator)],
          //   [txt("Stop Gen.")],
          // ),
          // hace(
          //   btn,
          //   [],
          //   "clearContentBtn_" ++ groupId,
          //   [("click", ClearContent(group |> PM.PeersGroup.id))],
          //   [txt("Clear content")],
          // ),
        ],
      ),
      hac(
        div,
        [(cls, "valbox")],
        [
          hac(div, [(cls, "valbox__bold")], [txt("Members:")]),
          ...group
             |> PM.PeersGroup.foldPeersInGroup(
                  (acc, peerInGroup) =>
                    [viewPeerInGroup(group, peerInGroup), ...acc],
                  [],
                ),
        ],
      ),
      hac(
        div,
        [(cls, "valbox")],
        [
          hac(div, [(cls, "valbox__bold")], [txt("Content:")]),
          ...group
             |> PM.PeersGroup.content
             |> foldContentItems(
                  (acc, item) => [viewContentItem(item), ...acc],
                  [],
                )
             |?>> List.rev
             |?>> (
               list =>
                 list == [] ? [viewContentItem({js|– empty –|js})] : list
             )
             |? [viewContentItem("'items' key is missing or is not a List")],
        ],
      ),
    ],
  );
};

let viewGroups = groups =>
  vSection(
    "Groups",
    [
      hc(
        div,
        [
          hace(
            "input",
            [(placeholder, "Group ID")],
            "addGroupInput",
            [],
            [],
          ),
          hace(btn, [], "addGroupBtn", [], [txt("Add")]),
        ],
      ),
      hc(
        div,
        [
          hace(
            "input",
            [(placeholder, "Peer ID")],
            "peerInGroupPeerInput",
            [],
            [],
          ),
          hace(
            "input",
            [(placeholder, "Group ID")],
            "peerInGroupGroupInput",
            [],
            [],
          ),
          hace(
            "select",
            [],
            "peerInGroupPermsSelect",
            [],
            [
              hac(
                "option",
                [("value", "crmr")],
                [txt("Content: R, Members: R")],
              ),
              hac(
                "option",
                [("value", "crwmr")],
                [txt("Content: RW, Members: R")],
              ),
              hac(
                "option",
                [("value", "crwmrw")],
                [txt("Content: RW, Members: RW")],
              ),
            ],
          ),
          hace(btn, [], "addPeerInGroupBtn", [], [txt("Add")]),
          hace(btn, [], "updatePeerInGroupBtn", [], [txt("Update")]),
        ],
      ),
      ...groups
         |> PM.PeersGroups.fold(
              (acc, group) => [viewGroup(group), ...acc],
              [],
            ),
    ],
  );

let p2pView = (m: PM.DbState.t, r: PM.RuntimeState.t) => {
  let thisPeerId =
    m |> PM.DbState.thisPeer |> PM.ThisPeer.id |> PM.Peer.Id.toString;

  let ssState =
    switch (
      r |> PM.RuntimeState.signalServer |> PM.SignalServer.connectionState
    ) {
    | Connecting => "connecting"
    | Connected => "CONNECTED"
    };
  let peers = m |> PM.DbState.peers;
  let groups = m |> PM.DbState.groups;
  hac(
    div,
    [(cls, "sections")],
    [
      vSection("This peer", [vKeyVal("ID", thisPeerId)]),
      vSection(
        "Signal Server",
        [
          vKeyVal(
            "URL",
            r |> PM.RuntimeState.signalServer |> PM.SignalServer.url,
          ),
          vKeyVal("Connection state", ssState),
        ],
      ),
      viewPeers(peers),
      viewGroups(groups),
    ],
  );
};

let view = (m: model) =>
  switch (m.p2p |> PM.State.classify) {
  | WaitingForDbAndIdentity(_signalServer) =>
    hc(div, ["Waiting for IDB and identity..." |> txt])
  | HasIdentity(dbState, runtimeState) => p2pView(dbState, runtimeState)
  };

/* INIT, UPDATE */

let contentInitializer = crdt =>
  crdt
  |> PM.Crdt.change("Init", root =>
       PM.Crdt.Json.(root |> Map.add("items", List.create() |> List.toJson))
     );

let init = () => {
  let p2pConfig = PM.InitConfig.make(~contentInitializer, ());
  let (p2p, p2pCmd) = PM.init(p2pConfig);

  (
    {
      p2p,
      counter: 42,
      // itemsGeneratorGroup: None,
      itemsGenerator: InputEmulator.create(),
    },
    p2pCmd |> Cmd.map(pMMsg),
  );
};

let update = model =>
  fun
  | PMMsg(msg) => {
      let (newP2P, p2pCmd) = PM.update(model.p2p, msg);
      ({...model, p2p: newP2P}, p2pCmd |> Cmd.map(pMMsg));
    }
  | Increment => ({...model, counter: model.counter + 1}, Cmd.none);
// | StartItemsGenerator(groupId) => {
//     let generatedItemPrefix = getThisPeerIdStart(model);
//     (
//       {...model, itemsGeneratorGroup: Some(groupId)},
//       model.itemsGenerator
//       |> InputEmulator.Cmds.start(itemGenerated, generatedItemPrefix),
//     );
//   }
// | StopItemsGenerator => (
//     {...model, itemsGeneratorGroup: None},
//     model.itemsGenerator |> InputEmulator.Cmds.stop,
//   )
// | ItemGenerated(text) =>
//   switch (
//     model.itemsGeneratorGroup,
//     model.itemsGeneratorGroup
//     |?> (groupId => model |> getGroupContentOpt(groupId)),
//   ) {
//   | (Some(itemsGeneratorGroup), Some(content)) =>
//     let newContent =
//       content
//       |> PM.Crdt.change("Add item", root =>
//            PM.Crdt.Json.(
//              switch (root |> Map.get("items") |?> List.ofJson) {
//              | Some(list) =>
//                root
//                |> Map.add(
//                     "items",
//                     list |> List.prepend(string(text)) |> List.toJson,
//                   )
//              | None => root
//              }
//            )
//          );
//     let (newP2P, p2pCmd) =
//       PM.update(
//         model.p2p,
//         PM.Msg.updateGroupContent(itemsGeneratorGroup, newContent),
//       );
//     ({...model, p2p: newP2P}, p2pCmd |> Cmd.map(pMMsg));
//   | _ => (model, Cmd.none)
//   }
// | ClearContent(groupId) =>
//   switch (model |> getGroupContentOpt(groupId)) {
//   | Some(content) =>
//     let newContent = removeAllGroupContentItems(content);
//     let (newP2P, p2pCmd) =
//       PM.update(model.p2p, PM.Msg.updateGroupContent(groupId, newContent));
//     ({...model, p2p: newP2P}, p2pCmd |> Cmd.map(pMMsg));
//   | None => (model, Cmd.none)
//   };

let renderSub = (model, pushMsg) => {
  let (html, evts) = model |> view;
  renderHtml(html);

  evts
  |> List.iter(((id, eventType, msg)) =>
       addEvent(id, eventType, () => pushMsg(msg))
     );
};

let pushOpt: ref(option(msg => unit)) = ref(None);

[@bs.get] external getInputValue: Dom.element => string = "value";

let decodePerms =
  fun
  | "crmr" => Some(PM.PeerInGroup.ReadContentAndMembers)
  | "crwmr" => Some(PM.PeerInGroup.WriteContent(ReadMembers))
  | "crwmrw" => Some(PM.PeerInGroup.WriteContent(WriteMembers))
  | _ => None;

let linkEvents = () =>
  Webapi.Dom.(
    switch (
      pushOpt^,
      document |> Document.getElementById("addPeerIdInput"),
      document |> Document.getElementById("addPeerIdBtn"),
      document |> Document.getElementById("peerInGroupPeerInput"),
      document |> Document.getElementById("peerInGroupGroupInput"),
      document |> Document.getElementById("peerInGroupPermsSelect"),
      document |> Document.getElementById("addPeerInGroupBtn"),
      document |> Document.getElementById("updatePeerInGroupBtn"),
      document |> Document.getElementById("addGroupInput"),
      document |> Document.getElementById("addGroupBtn"),
    ) {
    | (
        Some(pushMsg),
        Some(addPeerIdInput),
        Some(addPeerIdBtn),
        Some(peerInGroupPeerInput),
        Some(peerInGroupGroupInput),
        Some(peerInGroupPermsSelect),
        Some(addPeerInGroupBtn),
        Some(updatePeerInGroupBtn),
        Some(addGroupInput),
        Some(addGroupBtn),
      ) =>
      addPeerIdBtn
      |> Element.addClickEventListener(_ =>
           switch (addPeerIdInput |> getInputValue |> PM.Peer.Id.ofString) {
           | Some(peerId) => pushMsg(PMMsg(PM.Msg.addPeer(peerId, "")))
           | None => ()
           }
         );

      addGroupBtn
      |> Element.addClickEventListener(_ =>
           switch (addGroupInput |> getInputValue |> PM.PeersGroup.Id.ofString) {
           | Some(groupId) =>
             pushMsg(
               PMMsg(PM.Msg.addGroup(groupId, "", contentInitializer)),
             )
           | _ => ()
           }
         );

      addPeerInGroupBtn
      |> Element.addClickEventListener(_ =>
           switch (
             peerInGroupPeerInput |> getInputValue |> PM.Peer.Id.ofString,
             peerInGroupGroupInput
             |> getInputValue
             |> PM.PeersGroup.Id.ofString,
             peerInGroupPermsSelect |> getInputValue |> decodePerms,
           ) {
           | (Some(peerId), Some(groupId), Some(perms)) =>
             pushMsg(PMMsg(PM.Msg.addPeerToGroup(peerId, groupId, perms)))
           | _ => ()
           }
         );

      updatePeerInGroupBtn
      |> Element.addClickEventListener(_ =>
           switch (
             peerInGroupPeerInput |> getInputValue |> PM.Peer.Id.ofString,
             peerInGroupGroupInput
             |> getInputValue
             |> PM.PeersGroup.Id.ofString,
             peerInGroupPermsSelect |> getInputValue |> decodePerms,
           ) {
           | (Some(peerId), Some(groupId), Some(perms)) =>
             pushMsg(
               PMMsg(PM.Msg.updatePeerPermissions(peerId, groupId, perms)),
             )
           | _ => ()
           }
         );

    | _ => ()
    }
  );

let app =
  BlackTea.Store.create(
    ~init,
    ~update,
    /* ~subscriptions=model => stateLogger(model), */
    ~subscriptions=
      model => {
        makeGlobal("model", model);
        switch (pushOpt^) {
        | Some(pushMsg) =>
          renderSub(model, pushMsg);
          linkEvents();
        | None => ()
        };
        PM.subscriptions(model.p2p) |> BlackTea.Sub.map(pMMsg);
      },
    ~shutdown=_model => Cmds.none,
  );

pushOpt := Some(app.pushMsg);