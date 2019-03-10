let elementArrayWithDefaultMsg = (text, arr) =>
  arr |> Array.length > 0 ? arr |> ReasonReact.array : <LonelyMessage text />;

let truncate = (text: string, maxChars) =>
  (text |> Js.String.substring(~from=0, ~to_=maxChars)) ++ {js|…|js};