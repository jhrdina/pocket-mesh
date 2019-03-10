open Json.Infix;

Nocrypto_entropy_lwt.initialize();

type key = NocryptoRe.Rsa.pub_;
type jwkRsa = {
  e: string,
  kty: string,
  n: string,
};
type jwk =
  | Rsa(jwkRsa);

let digest = Nocrypto.Hash.SHA256.digest;

let fingerprintForRSAJWK = jwk =>
  switch (jwk) {
  | Rsa({e, kty, n}) =>
    let str =
      Json.(
        Object([
          ("e", String(e)),
          ("kty", String(kty)),
          ("n", String(n)),
        ])
        |> stringify
      )
      /* TODO: Make Json.stringify omit spaces, remove this dangerous workaround */
      |> Str.global_replace(Str.regexp_string(" "), "");

    Cstruct.of_string(str)
    |> digest
    |> Nocrypto.Base64.encode
    |> Cstruct.to_string;
  };

let base64UrlToBase64 = s => {
  let s =
    s
    |> Str.global_replace(Str.regexp("-"), "+")
    |> Str.global_replace(Str.regexp("_"), "/");

  switch (String.length(s) mod 4) {
  | 0 => Some(s)
  | 2 => Some(s ++ "==")
  | 3 => Some(s ++ "=")
  | _ => None
  };
};

let base64UrlToZ = base64Url => {
  base64Url
  |> base64UrlToBase64
  |?>> Cstruct.of_string
  |?> Nocrypto.Base64.decode
  |?>> Nocrypto.Numeric.Z.of_cstruct_be;
};

let stringToJwk = jwkStr => {
  let json = jwkStr |> JsonUtils.parseOpt;
  switch (json |?> Json.get("kty") |?> Json.string) {
  | Some("RSA" as kty) =>
    switch (
      json |?> Json.get("e") |?> Json.string,
      json |?> Json.get("n") |?> Json.string,
    ) {
    | (Some(e), Some(n)) => Some(Rsa({e, kty, n}))
    | _ => None
    }
  | Some(_)
  | None => None
  };
};

let jwkToPublicKey: jwk => option(key) =
  jwk =>
    switch (jwk) {
    | Rsa(jwkRsa) =>
      switch (jwkRsa.e |> base64UrlToZ, jwkRsa.n |> base64UrlToZ) {
      | (Some(e), Some(n)) => Some({Nocrypto.Rsa.e, n})
      | _ => None
      }
    };

let verify = (publicKey, signatureBase64, strToVerify) => {
  switch (
    Nocrypto.Base64.decode(Cstruct.of_string(signatureBase64))
    |?> Nocrypto.Rsa.PKCS1.sig_decode(~key=publicKey)
  ) {
  | Some(signedHashPadded) =>
    switch (Cstruct.shift(signedHashPadded, 19)) {
    | signedHash =>
      let strHash = Cstruct.of_string(strToVerify) |> digest;
      Cstruct.equal(signedHash, strHash);
    | exception (Invalid_argument(_)) => false
    }

  | None => false
  };
};