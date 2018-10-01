type t;

type role =
  | Initiator
  | Acceptor;

type options = {role};

let create: options => t;

let send: (t, string) => unit;
let signal: (t, string) => unit;

let setOnSignal: (t, string => unit) => unit;
let setOnData: (t, string => unit) => unit;
let setOnConnect: (t, unit => unit) => unit;
let setOnError: (t, string => unit) => unit;