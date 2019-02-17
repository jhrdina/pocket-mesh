open Js.Promise;
open BlackTea;

let generateKeyPair:
  (Result.t(SimpleCrypto.keyPair, exn) => Msgs.t) => Cmd.t(Msgs.t) =
  Cmds.wrapResPromise(SimpleCrypto.generateKeyPair);
let exportPublicKey = publicKey =>
  Cmds.wrapResPromise(() => SimpleCrypto.publicKeyToJwk(publicKey));
let importAndFingerprintKey = jwk =>
  Cmds.wrapPairPromise(() =>
    all2((
      SimpleCrypto.fingerprintForRSAJWK(jwk),
      SimpleCrypto.jwkToPublicKey(jwk),
    ))
  );