let elementArrayWithDefaultMsg = (text, arr) =>
  arr |> Array.length > 0 ? arr |> ReasonReact.array : <LonelyMessage text />;

let truncate = (maxChars, text: string) =>
  (text |> Js.String.substring(~from=0, ~to_=maxChars)) ++ {js|…|js};

let getPeerGroupVisibleName = group =>
  PM.PeersGroup.(
    group |> alias != "" ? group |> alias : group |> id |> Id.toString
  );

let getPeerVisibleName = (~idMaxChars=0, peer) =>
  PM.Peer.(
    if (peer |> alias != "") {
      peer |> alias;
    } else {
      let idStr = peer |> id |> Id.toString;
      idMaxChars == 0 ? idStr : idStr |> truncate(idMaxChars);
    }
  );

let quote = text => {js|“|js} ++ text ++ {js|”|js};