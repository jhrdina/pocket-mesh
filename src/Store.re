/* TODO: Remove for production */
[%%debugger.chrome];

open BlackTea;

/* TYPES */

type t =
  | WaitingForDbAndIdentity(
      Db.t,
      RuntimeState.InitConfig.t,
      SignalServerState.t,
    )
  | HasIdentity(Db.t, DbState.t, RuntimeState.t);

/* UPDATE */

let init = (initConfig: RuntimeState.InitConfig.t) => {
  let (db, dbCmd) = Db.init();
  let signalServer = SignalServerState.init(initConfig.signalServerUrl);
  (WaitingForDbAndIdentity(db, initConfig, signalServer), dbCmd);
};

let update = (model, msg) => {
  Js.log(msg);
  switch (msg, model) {
  | (
      Db.InitializationComplete(dbState),
      WaitingForDbAndIdentity(db, initConfig, signalServer),
    ) =>
    let (runtimeState, runtimeStateCmd) =
      RuntimeState.init(~initConfig, ~dbState, ~signalServer);
    (HasIdentity(db, dbState, runtimeState), runtimeStateCmd);

  // | (Db.Changed(newDbState), HasIdentity(db, dbState, runtimeState)) =>
  //   HasIdentity(db, newDbState, runtimeState)

  | (msg, HasIdentity(db, dbState, runtimeState)) =>
    let (dbState, dbStateCmd) = DbState.update(msg, dbState);
    let (runtimeState, runtimeStateCmd) =
      RuntimeState.update(dbState, msg, runtimeState);
    let (db, dbCmd) =
      Db.update(
        ~dbState=Some(dbState),
        ~initContent=runtimeState.initConfig.contentInitializer,
        msg,
        db,
      );
    (
      HasIdentity(db, dbState, runtimeState),
      Cmd.batch([dbStateCmd, runtimeStateCmd, dbCmd]),
    );

  | (msg, WaitingForDbAndIdentity(db, initConfig, signalServer)) =>
    let (db, dbCmd) =
      Db.update(
        ~dbState=None,
        ~initContent=initConfig.contentInitializer,
        msg,
        db,
      );
    let (signalServer, signalServerCmd) =
      SignalServerState.update(signalServer, msg);
    (
      WaitingForDbAndIdentity(db, initConfig, signalServer),
      Cmd.batch([dbCmd, signalServerCmd]),
    );
  };
};

[@bs.set_index]
external makeGlobal: (Webapi.Dom.Window.t, string, 'a) => unit = "";
let makeGlobal = (name, value) => makeGlobal(Webapi.Dom.window, name, value);

let subscriptions = model => {
  makeGlobal("model", model);
  switch (model) {
  | WaitingForDbAndIdentity(_, _, signalServer) =>
    SignalServerState.subscriptions(signalServer)
  | HasIdentity(_db, _dbState, runtimeState) =>
    RuntimeState.subscriptions(runtimeState)
  };
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
  //       PeerGroups.defaultGroupId,
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