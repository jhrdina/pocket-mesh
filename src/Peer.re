/**
  Represents persistent part of peer
 */

type t = {
  id: PeerId.t,
  publicKey: option(SimpleCrypto.key),
  alias: string,
};