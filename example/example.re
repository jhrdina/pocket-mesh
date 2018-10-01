let simpleCryptoDemo = () => {
  let strToSign = "Pleeeee";
  let then_ = Js.Promise.then_;
  let thenl = cb =>
    Js.Promise.then_(v => {
      Js.log(v);
      cb(v);
    });

  SimpleCrypto.generateKeyPair()
  |> then_(keyPair =>
       SimpleCrypto.sign(keyPair.SimpleCrypto.privateKey, strToSign)
       |> thenl(signature =>
            SimpleCrypto.verify(keyPair.publicKey, signature, "hasdf")
          )
       |> thenl(_ => SimpleCrypto.publicKeyToJwk(keyPair.publicKey))
     )
  |> thenl(jwk =>
       SimpleCrypto.fingerprintForRSAJWK(jwk)
       |> thenl(_ => SimpleCrypto.jwkToPublicKey(jwk))
     )
  |> thenl(SimpleCrypto.publicKeyToJwk)
  |> thenl(SimpleCrypto.fingerprintForRSAJWK)
  |> thenl(_ => Js.Promise.resolve());
};

simpleCryptoDemo();