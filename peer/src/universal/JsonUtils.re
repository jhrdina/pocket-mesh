open Json;

let parseOpt = str =>
  switch (str |> parse) {
  | json => Some(json)
  | exception _ => None
  };