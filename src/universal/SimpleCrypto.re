open WebapiExtra;

type key = Dom.cryptoKey;
type keyPair = {
  fingerprint: string,
  publicKey: key,
  privateKey: key,
};
type jwk = Dom.jsonWebKey;

let algorithm = "RSASSA-PKCS1-v1_5";
let hash = "SHA-256";
let subtle = Dom.crypto->Dom.Crypto.subtle;

exception JWKMissingKeys(string);
exception InternalError(string);

/* https://tools.ietf.org/html/rfc7638 */
let fingerprintForRSAJWK = (jwk: Dom.jsonWebKey) =>
  switch (jwk##e, jwk##kty, jwk##n) {
  | (Some(e), Some(kty), Some(n)) =>
    switch (Js.Json.stringifyAny({"e": e, "kty": kty, "n": n})) {
    | Some(str) =>
      str
      |> SimpleEncoding.stringToArrayBuffer
      |> Dom.crypto->Dom.Crypto.subtle->Dom.SubtleCrypto.digest(hash)
      |> Js.Promise.then_(hashBuffer =>
           Js.Promise.resolve(SimpleEncoding.arrayBufferToBase64(hashBuffer))
         )
    | None => Js.Promise.reject @@ InternalError("Stringify failed.")
    }
  | _ =>
    Js.Promise.reject @@
    JWKMissingKeys("JWK must contain all of the following keys: e, kty, n.")
  };

let jwkToPublicKey = jwk =>
  subtle
  ->Dom.SubtleCrypto.importKey(
      jwk,
      Dom.RsaHashedImportParams.make(
        ~name=algorithm,
        ~hash=Dom.HashAlgorithmIdentifier.make(~name=hash),
      ),
      true,
      [|`Verify|],
    );

let publicKeyToJwk = key => subtle->Dom.SubtleCrypto.exportKey(key);

let jwkToString = jwk =>
  switch (jwk |> Js.Json.stringifyAny) {
  | Some(str) => str
  | None => ""
  };

let stringToJwk: string => jwk = [%bs.raw str => "return JSON.parse(str);"];

let generateKeyPair = () =>
  subtle
  ->Dom.SubtleCrypto.generateKeyPair(
      Dom.RsaHashedKeyGenParams.make(
        ~name=algorithm,
        ~modulusLength=2048,
        ~publicExponent=Js.Typed_array.Uint8Array.make([|0x01, 0x00, 0x01|]),
        ~hash=Dom.HashAlgorithmIdentifier.make(~name=hash),
      ),
      false,
      [|`Sign, `Verify|],
    )
  |> Js.Promise.then_(keyPair =>
       publicKeyToJwk(keyPair->Dom.CryptoKeyPair.publicKey)
       |> Js.Promise.then_(fingerprintForRSAJWK)
       |> Js.Promise.then_(fingerprint =>
            Js.Promise.resolve({
              fingerprint,
              publicKey: keyPair->Dom.CryptoKeyPair.publicKey,
              privateKey: keyPair->Dom.CryptoKeyPair.privateKey,
            })
          )
     );

let sign = (privateKey, stringToSign) =>
  subtle
  ->Dom.SubtleCrypto.sign(
      algorithm,
      privateKey,
      SimpleEncoding.stringToArrayBuffer(stringToSign),
    )
  |> Js.Promise.then_(signatureBuffer =>
       Js.Promise.resolve(
         SimpleEncoding.arrayBufferToBase64(signatureBuffer),
       )
     );

let verify = (publicKey, signatureBase64, strToVerify) =>
  subtle
  ->Dom.SubtleCrypto.verify(
      algorithm,
      publicKey,
      SimpleEncoding.base64ToArrayBuffer(signatureBase64),
      SimpleEncoding.stringToArrayBuffer(strToVerify),
    );