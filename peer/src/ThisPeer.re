// TYPES

type t = {
  id: PeerId.t,
  publicKey: SimpleCrypto.key,
  privateKey: SimpleCrypto.key,
};

// QUERIES

let id = t => t.id;

// UPDATE

let init = t => t;

let update = (model, msg) =>
  switch (msg) {
  | _ => (model, Cmds.none)
  };