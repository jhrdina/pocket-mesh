// TYPES

type iceCredentials =
  SimpleRTC.iceCredentials = {
    username: string,
    credential: string,
  };
type iceServer =
  SimpleRTC.iceServer =
    | Basic(string) | WithCredentials(string, iceCredentials);

type t = {
  defaultGroupAlias: string,
  contentInitializer: PeerGroup.AM.t => PeerGroup.AM.t,
  signalServerUrl: string,
  iceServers: list(iceServer),
};

// CONSTANTS

let defaultSignalServerUrl = "ws://localhost:7777";
let defaultGroupAlias = "My group";
let defaultIceServers: list(iceServer) = [
  Basic("stun:stun.l.google.com:19302"),
];

let make =
    (
      ~contentInitializer=crdt => crdt,
      ~signalServerUrl=defaultSignalServerUrl,
      ~defaultGroupAlias=defaultGroupAlias,
      ~iceServers=defaultIceServers,
      (),
    ) => {
  defaultGroupAlias,
  contentInitializer,
  signalServerUrl,
  iceServers,
};