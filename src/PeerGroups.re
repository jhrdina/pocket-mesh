type t = list(Types.peerGroup);

let empty = () => [];

let addPeerGroup = (peerGroup, t) => [peerGroup, ...t];
let getFirstId: t => option(string) =
  fun
  | [first, ..._] => Some(first.id)
  | [] => None;

let update = (id, updateFn, t) =>
  t
  |> List.map((item: Types.peerGroup) =>
       item.id === id ? updateFn(item) : item
     );