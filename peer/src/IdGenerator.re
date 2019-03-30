open BsUuid;
open BlackTea;

let generate = () => Uuid.V4.create() |> Uuid.V4.toString;

let generateCmd = uuidToMsg =>
  Cmd.call(callbacks => callbacks^.enqueue(generate() |> uuidToMsg));

let generateGroupId = () => generate() |> PeerGroup.Id.ofStringExn;

let generateGroupIdCmd = uuidToMsg =>
  Cmd.call(callbacks => callbacks^.enqueue(generateGroupId() |> uuidToMsg));