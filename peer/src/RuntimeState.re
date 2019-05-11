open BlackTea;

// TYPES

type t = {
  initConfig: InitConfig.t,
  signalChannel: SignalChannel.t,
  signalChannelAuth: SignalChannelAuth.t,
  peersConnections: PeersConnections.t,
  peersGroupsSynchronizer: PeersGroupsSynchronizer.t,
  peersKeysFetcherAndSender: PeersKeysFetcherAndSender.t,
  peersStatuses: PeersStatuses.t,
  // signalVerifierAndSigner,
  thisPeerKeyExporter: ThisPeerKeyExporter.t,
};

let init = (~dbState: DbState.t, ~signalChannel, ~initConfig) => {
  let thisPeerKeyExporter =
    ThisPeerKeyExporter.init(~thisPeer=dbState.thisPeer);
  let peersStatuses = PeersStatuses.init();
  let (peersKeysFetcherAndSender, peersKeysFetcherAndSenderCmd) =
    PeersKeysFetcherAndSender.init(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~thisPeerKeyExporter,
      ~peersStatuses,
    );
  let peersConnections =
    PeersConnections.init(
      ~peers=dbState.peers,
      ~peersStatuses,
      ~peersGroups=dbState.peersGroups,
    );

  let (peersGroupsSynchronizer, peersGroupsSynchronizerCmd) =
    PeersGroupsSynchronizer.init(
      ~peersGroups=dbState.peersGroups,
      ~peersConnections,
    );
  (
    {
      initConfig,
      signalChannel,
      signalChannelAuth: SignalChannelAuth.init(),
      peersConnections,
      peersGroupsSynchronizer,
      peersKeysFetcherAndSender,
      peersStatuses,
      // signalVerifierAndSigner,
      thisPeerKeyExporter,
    },
    Cmd.batch([peersKeysFetcherAndSenderCmd, peersGroupsSynchronizerCmd]),
  );
};

let update = (dbState: DbState.t, msg, model) => {
  let (signalChannel, signalChannelCmd) =
    SignalChannel.update(model.signalChannel, msg);
  let (thisPeerKeyExporter, thisPeerKeyExporterCmd) =
    ThisPeerKeyExporter.update(
      ~thisPeer=dbState.thisPeer,
      msg,
      model.thisPeerKeyExporter,
    );
  let (signalChannelAuth, signalChannelAuthCmd) =
    SignalChannelAuth.update(
      ~peers=dbState.peers,
      ~signalChannel,
      ~thisPeer=dbState.thisPeer,
      ~thisPeerKeyExporter,
      msg,
      model.signalChannelAuth,
    );
  let signalVerifierCmd =
    SignalVerifier.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      msg,
    );
  let (peersStatuses, peersStatusesCmd) =
    PeersStatuses.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~signalChannelAuth,
      msg,
      model.peersStatuses,
    );
  let (peersKeysFetcherAndSender, peersKeysFetcherAndSenderCmd) =
    PeersKeysFetcherAndSender.update(
      ~thisPeer=dbState.thisPeer,
      ~peers=dbState.peers,
      ~thisPeerKeyExporter,
      ~peersStatuses,
      msg,
      model.peersKeysFetcherAndSender,
    );
  let (peersConnections, peersConnectionsCmd) =
    PeersConnections.update(
      ~peers=dbState.peers,
      ~thisPeer=dbState.thisPeer,
      ~peersGroups=dbState.peersGroups,
      ~peersStatuses,
      msg,
      model.peersConnections,
    );

  let (peersGroupsSynchronizer, peersGroupsSynchronizerCmd) =
    PeersGroupsSynchronizer.update(
      ~peersGroups=dbState.peersGroups,
      ~peersConnections,
      msg,
      model.peersGroupsSynchronizer,
    );

  // let (offerChangesDebouncer, offerChangesDebouncerCmd) =
  //   switch (msg) {
  //   | OfferChangesDebouncerMsg(msg) =>
  //     Debouncer.update(
  //       d => d,
  //       model.offerChangesDebouncer,
  //       msg,
  //       Msgs.offerChangesDebouncerMsg,
  //     )
  //   | _ => (model.offerChangesDebouncer, Cmds.none)
  //   };
  (
    {
      initConfig: model.initConfig,
      signalChannel,
      signalChannelAuth,
      peersConnections,
      peersGroupsSynchronizer,
      peersKeysFetcherAndSender,
      peersStatuses,
      // signalVerifierAndSigner,
      thisPeerKeyExporter,
    },
    Cmd.batch([
      signalChannelCmd,
      signalChannelAuthCmd,
      signalVerifierCmd,
      peersStatusesCmd,
      // debugCmd,
      // offerChangesDebouncerCmd,
      peersGroupsSynchronizerCmd,
      peersConnectionsCmd,
      thisPeerKeyExporterCmd,
      peersKeysFetcherAndSenderCmd,
    ]),
  );
};

let subscriptions = model =>
  Sub.batch([
    SignalChannel.subscriptions(model.signalChannel),
    ThisPeerKeyExporter.subscriptions(model.thisPeerKeyExporter),
    PeersConnections.subscriptions(
      ~initConfig=model.initConfig,
      model.peersConnections,
    ),
  ]);