open WebapiExtra;

let algorithm = "RSASSA-PKCS1-v1_5";
let hash = "SHA-256";

let arrayBufferToBase64 = buffer =>
  Js.Typed_array.(
    Uint8Array.fromBuffer(buffer)
    |> Uint8Array.reduce(
         (. binary, b) => binary ++ Js.String.fromCharCode(b),
         "",
       )
    |> Webapi.Base64.btoa
    |> Js.Promise.resolve
  );

/* let base64ToArrayBuffer = base64string => */

let stringToArrayBuffer = str =>
  Dom.TextEncoder.create()
  ->Dom.TextEncoder.encode(str)
  ->Js.Typed_array.Uint8Array.buffer;

exception JWKMissingKeys(string);
exception InternalError(string);
/* https://tools.ietf.org/html/rfc7638 */
let fingerprintForRSAJWK = (jwk: Dom.jsonWebKey) =>
  switch (jwk##e, jwk##kty, jwk##n) {
  | (Some(e), Some(kty), Some(n)) =>
    switch (Js.Json.stringifyAny({"e": e, "kty": kty, "n": n})) {
    | Some(str) =>
      str
      |> stringToArrayBuffer
      |> Dom.crypto->Dom.Crypto.subtle->Dom.SubtleCrypto.digest(hash)
      |> Js.Promise.then_(digestArrayBuffer =>
           Js.Promise.resolve(arrayBufferToBase64(digestArrayBuffer))
         )
    | None => Js.Promise.reject @@ InternalError("Stringify failed.")
    }
  | _ =>
    Js.Promise.reject @@
    JWKMissingKeys("JWK must contain all of the following keys: e, kty, n.")
  };

let subtle = Dom.crypto->Dom.Crypto.subtle;

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
    );

let sign = (privateKey, stringToSign) =>
  subtle
  ->Dom.SubtleCrypto.sign(
      algorithm,
      privateKey,
      stringToArrayBuffer(stringToSign),
    )
  |> Js.Promise.then_(arrayBufferToBase64);

let verify = (publicKey, signature, strToVerify) =>
  subtle
  ->Dom.SubtleCrypto.verify(
      algorithm,
      publicKey,
      signature,
      stringToArrayBuffer(strToVerify),
    );

let run = () => {
  let thenl = cb =>
    Js.Promise.then_(v => {
      Js.log(v);
      cb(v);
    });
  ();
  /* |> then_(keyPair =>
          subtle
          |> then_(signature =>

             )
          |> thenl(_ =>
               fingerprintForRSAJWK(
                 Dom.JsonWebKey.create(~e="e", ~n="n", ~kty="kty", ()),
               )
             )
          |> thenl(_ =>
               subtle
               ->Dom.SubtleCrypto.exportKey(keyPair->Dom.CryptoKeyPair.publicKey)
             )
        )
     |> thenl(jwk =>
          fingerprintForRSAJWK(jwk)
          |> thenl(_ =>
               subtle
               ->Dom.SubtleCrypto.importKey(
                   jwk,
                   Dom.RsaHashedImportParams.make(
                     ~name=algorithm,
                     ~hash=Dom.HashAlgorithmIdentifier.make(~name=hash),
                   ),
                   true,
                   [|`Verify|],
                 )
             )
        )
     |> thenl(subtle->Dom.SubtleCrypto.exportKey)
     |> thenl(fingerprintForRSAJWK)
     |> thenl(Js.Promise.resolve); */
};