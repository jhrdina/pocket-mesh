module Impl: {
  type t;
  exception InvalidPeerIdString;
  let compare: (t, t) => int;
  let ofString: string => option(t);
  let ofStringExn: string => t;
  let toString: t => string;
} = {
  type t = string;
  exception InvalidPeerIdString;
  let compare = compare;
  /* TODO: Only non-empty strings */
  let ofString = str => Some(str);
  let ofStringExn = str =>
    switch (ofString(str)) {
    | Some(peerId) => peerId
    | None => raise(InvalidPeerIdString)
    };
  let toString = t => t;
};

include Impl;
module Map = {
  include Diff.Map.Make(Impl);
  let findOpt = (key, t) =>
    switch (find(key, t)) {
    | v => Some(v)
    | exception Not_found => None
    };
};
module Set = Diff.Set.Make(Impl);