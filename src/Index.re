/* Temporary */

module Automerge = {
  module Doc = {
    type t;
  };
};

/* ++++++++++++++++ */

type peerId = string;

type permission =
  | Read
  | Write
  | ReadWrite;

type groupPermissions = {
  content: permission,
  permissions: permission,
};

type peerInGroup = {
  id: peerId,
  permissions: groupPermissions,
};

type peerGroup = {
  peers: list(peerInGroup),
  content: Automerge.Doc.t,
};

type thisPeer = {
  privateKey: WebapiExtra.Dom.cryptoKey,
  publicKey: WebapiExtra.Dom.cryptoKey,
  id: peerId,
};

type rootState = {
  peerGroups: list(peerGroup),
  thisPeer,
};