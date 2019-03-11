module Impl: {
  type t;
  exception InvalidPeerIdString;
  let compare: (t, t) => int;
  let equal: (t, t) => bool;
  let ofString: string => option(t);
  let ofStringExn: string => t;
  let toString: t => string;
} = {
  type t = string;
  exception InvalidPeerIdString;
  let compare = compare;
  let equal = (==);
  let ofString = str => {
    let trimmed = str |> String.trim;
    trimmed != "" ? Some(trimmed) : None;
  };
  let ofStringExn = str =>
    switch (ofString(str)) {
    | Some(peerId) => peerId
    | None => raise(InvalidPeerIdString)
    };
  let toString = t => t;
};

include Impl;
module Map = {
  include OcamlDiff.Map.Make(Impl);
  let findOpt = (key, t) =>
    switch (find(key, t)) {
    | v => Some(v)
    | exception Not_found => None
    };
};
module Set = OcamlDiff.Set.Make(Impl);