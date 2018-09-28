module type T = {
  module Socket: {
    type t;
    let emit: (t, string) => Lwt.t(unit);
    let setOnMessage: ((string, string) => unit, t) => t;
    let setOnDisconnect: (unit => unit, t) => t;
  };
  let run: (~port: int, ~onConnection: Socket.t => Socket.t) => unit;
};