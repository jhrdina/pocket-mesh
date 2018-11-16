module Impl: {
  type t;
  let compare: (t, t) => int;
  let ofString: string => option(t);
  let toString: t => string;
} = {
  type t = string;
  let compare = compare;
  /* TODO: Only non-empty strings */
  let ofString = str => Some(str);
  let toString = t => t;
};

include Impl;
module Map = Map.Make(Impl);
module Set = Set.Make(Impl);