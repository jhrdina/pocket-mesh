module Impl = {
  type t = string;
  let compare = compare;
};

include Impl;
module Map = Map.Make(Impl);
module Set = Set.Make(Impl);