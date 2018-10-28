open Js.Promise;
open BlackTea;

let generateKeyPair:
  (SimpleCrypto.keyPair => Msgs.t, exn => Msgs.t) => Cmd.t(Msgs.t) =
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