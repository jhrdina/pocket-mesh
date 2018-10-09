open Js.Promise;
open BlackTea;

let generateKeyPair:
  (SimpleCrypto.keyPair => Msgs.t, Js.Promise.error => Msgs.t) =>
  Cmd.t(Msgs.t) =
  Cmds.wrapPromise(SimpleCrypto.generateKeyPair);
let exportPublicKey = publicKey =>
  Cmds.wrapPromise(() => SimpleCrypto.publicKeyToJwk(publicKey));
let importAndFingerprintKey = jwk =>
  Cmds.wrapPairPromise(() =>
    all2((
      SimpleCrypto.fingerprintForRSAJWK(jwk),
      SimpleCrypto.jwkToPublicKey(jwk),
    ))
  );