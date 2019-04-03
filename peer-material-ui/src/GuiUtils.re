let elementArrayWithDefaultMsg = (text, arr) =>
  arr |> Array.length > 0 ? arr |> ReasonReact.array : <LonelyMessage text />;

let truncate = (maxChars, text: string) =>
  maxChars == 0
    ? text
    : (text |> Js.String.substring(~from=0, ~to_=maxChars)) ++ {js|…|js};

let getPeerGroupVisibleName = group =>
  PM.PeersGroup.(
    group |> alias != "" ? group |> alias : group |> id |> Id.toString
  );

let getPeerVisibleName = (~idMaxChars=0, ~dbState, peerId) =>
  PM.(
    switch (dbState |> DbState.peers |> Peers.findOpt(peerId)) {
    | Some(peer) when peer |> Peer.alias != "" => peer |> Peer.alias
    | Some(_) => peerId |> Peer.Id.toString |> truncate(idMaxChars)
    | None when peerId == (dbState |> DbState.thisPeer |> ThisPeer.id) => "This peer"
    | None => peerId |> Peer.Id.toString |> truncate(idMaxChars)
    }
  );

let quote = text => {js|“|js} ++ text ++ {js|”|js};