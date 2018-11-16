type t = {
  id: PeerId.t,
  publicKey: SimpleCrypto.key,
  privateKey: SimpleCrypto.key,
};

let id = t => t.id;