/* TYPES */

type data =
  | String(string)
  | ArrayBuffer(Js.Typed_array.array_buffer);

type t;

type role =
  | Initiator
  | Acceptor;

type options = {role};

/* FUNCTIONS */

let create: options => t;

let send: (t, data) => unit;
let signal: (t, string) => unit;

let setOnSignal: (t, string => unit) => unit;
let setOnData: (t, data => unit) => unit;
let setOnConnect: (t, unit => unit) => unit;
let setOnError: (t, string => unit) => unit;
let setOnClose: (t, unit => unit) => unit;

let destroy: t => unit;