module Json = {
  include Json;

  let parseOpt = str =>
    switch (str |> parse) {
    | json => Some(json)
    | exception _ => None
    };
};