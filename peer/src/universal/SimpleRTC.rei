/* TYPES */

type data =
  | String(string)
  | ArrayBuffer(Js.Typed_array.array_buffer);

type t;

type role =
  | Initiator
  | Acceptor;

type iceCredentials = {
  username: string,
  credential: string,
};

type iceServer =
  | Basic(string)
  | WithCredentials(string, iceCredentials);

/* FUNCTIONS */

let create: (~role: role, ~iceServers: list(iceServer)) => t;

let send: (t, data) => unit;
let signal: (t, string) => unit;

let setOnSignal: (t, string => unit) => unit;
let setOnData: (t, data => unit) => unit;
let setOnConnect: (t, unit => unit) => unit;
let setOnError: (t, string => unit) => unit;
let setOnClose: (t, unit => unit) => unit;

let destroy: t => unit;