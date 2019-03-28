type key;
type jwk;
let fingerprintForRSAJWK: jwk => string;
let stringToJwk: string => option(jwk);
let jwkToPublicKey: jwk => option(key);
let verify: (key, string, string) => bool;

module Symmetric: {
  type key;
  let generateKeyStr: unit => string;
  let generateKey: unit => key;
  let keyOfString: string => option(key);
  let encrypt: (key, string) => string;
  let decrypt: (key, string) => option(string);
};