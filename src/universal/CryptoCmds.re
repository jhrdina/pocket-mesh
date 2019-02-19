open BlackTea;

let generateKeyPair:
  (Result.t(SimpleCrypto.keyPair, exn) => Msgs.t) => Cmd.t(Msgs.t) =
  Cmds.fromPromise(SimpleCrypto.generateKeyPair);