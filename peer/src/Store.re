/* DEBUG */
[%%debugger.chrome];

open BlackTea;

/* TYPES */

type t =
  | WaitingForDbAndIdentity(Db.t, RuntimeState.InitConfig.t, SignalChannel.t)
  | HasIdentity(Db.t, DbState.t, RuntimeState.t);

/* UPDATE */

let init = (initConfig: RuntimeState.InitConfig.t) => {
  let (db, dbCmd) = Db.init();
  let signalChannel = SignalChannel.init(initConfig.signalServerUrl);
  (WaitingForDbAndIdentity(db, initConfig, signalChannel), dbCmd);
};

let update = (model, msg) => {
  // Js.log(msg);
  switch (msg, model) {
  | (
      Db.InitializationComplete(dbState),
      WaitingForDbAndIdentity(db, initConfig, signalChannel),
    ) =>
    let (runtimeState, runtimeStateCmd) =
      RuntimeState.init(~initConfig, ~dbState, ~signalChannel);
    (HasIdentity(db, dbState, runtimeState), runtimeStateCmd);

  // | (Db.Changed(newDbState), HasIdentity(db, dbState, runtimeState)) =>
  //   HasIdentity(db, newDbState, runtimeState)

  | (msg, HasIdentity(db, dbState, runtimeState)) =>
    let (dbState, dbStateCmd) =
      DbState.update(
        ~initContent=runtimeState.initConfig.contentInitializer,
        ~defaultGroupAlias=runtimeState.initConfig.defaultGroupAlias,
        msg,
        dbState,
      );
    let (runtimeState, runtimeStateCmd) =
      RuntimeState.update(dbState, msg, runtimeState);
    let (db, dbCmd) = Db.update(~dbState=Some(dbState), msg, db);
    (
      HasIdentity(db, dbState, runtimeState),
      Cmd.batch([dbStateCmd, runtimeStateCmd, dbCmd]),
    );

  | (msg, WaitingForDbAndIdentity(db, initConfig, signalChannel)) =>
    let (db, dbCmd) = Db.update(~dbState=None, msg, db);
    let (signalChannel, signalChannelCmd) =
      SignalChannel.update(signalChannel, msg);
    (
      WaitingForDbAndIdentity(db, initConfig, signalChannel),
      Cmd.batch([dbCmd, signalChannelCmd]),
    );
  };
};

type window;
[@bs.val] external window: window = "";
[@bs.set_index] external makeGlobal: (window, string, 'a) => unit = "";
let makeGlobal = (name, value) => makeGlobal(window, name, value);

let subscriptions = model => {
  makeGlobal("model", model);
  let newSub =
    switch (model) {
    | WaitingForDbAndIdentity(_, _, signalChannel) =>
      Sub.batch([SignalChannel.subscriptions(signalChannel)])
    | HasIdentity(_db, _dbState, runtimeState) =>
      RuntimeState.subscriptions(runtimeState)
    };

  newSub;
};

let create = () => {
  /* let stateLogger = StateLogger.create(); */
  let initConfig =
    RuntimeState.InitConfig.make(
      ~contentInitializer=
        crdt =>
          crdt
          |> PeerGroup.AM.change("Init", root =>
               PeerGroup.AM.Json.(
                 root |> Map.add("items", List.create() |> List.toJson)
               )
             ),
      (),
    );
  let app =
    BlackTea.Store.create(
      ~init=() => init(initConfig),
      ~update,
      /* ~subscriptions=model => stateLogger(model), */
      ~subscriptions,
      ~shutdown=_model => Cmds.none,
    );

  makeGlobal("addPeer", (id, alias) =>
    app.pushMsg(Peers.AddPeer(id, alias))
  );
  // makeGlobal("addPeerToGroup", id =>
  //   app.pushMsg(
  //     AddPeerToGroup(
  //       id,
  //       PeersGroups.defaultGroupId,
  //       PeerGroup.ReadContentAndMembers,
  //     ),
  //   )
  // );
  // makeGlobal("sendToPeer", (id, msg) => app.pushMsg(SendToPeer(id, msg)));
  // makeGlobal("addPeerToGroupWithPerms", (peerId, groupId, permsStr) =>
  //   switch (permsStr |> PeerGroup.decodePermissions) {
  //   | Some(perms) => app.pushMsg(AddPeerToGroup(peerId, groupId, perms))
  //   | None => Js.log("Invalid permissions, try crmr, crwmr, or crwmrw")
  //   }
  // );
};