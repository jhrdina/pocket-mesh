type key;
type keyPair = {
  fingerprint: string,
  publicKey: key,
  privateKey: key,
};
type jwk;

exception JWKMissingKeys(string);
exception InternalError(string);
let fingerprintForRSAJWK: jwk => Js.Promise.t(string);
let jwkToPublicKey: jwk => Js.Promise.t(key);
let publicKeyToJwk: key => Js.Promise.t(jwk);

let generateKeyPair: unit => Js.Promise.t(keyPair);
let sign: (key, string) => Js.Promise.t(string);
let verify: (key, string, string) => Js.Promise.t(bool);