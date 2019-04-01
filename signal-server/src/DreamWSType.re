module type T = {
  module Socket: {
    type t;
    let compare: (t, t) => int;
    let emit: (t, string) => Lwt.t(unit);
    let setOnMessage: (string => unit, t) => t;
    let setOnDisconnect: (unit => unit, t) => t;
  };

  type tlsConfig = {
    cert: string,
    key: string,
  };

  let run:
    (
      ~port: int,
      ~tls: option(tlsConfig)=?,
      ~onConnection: Socket.t => Socket.t,
      unit
    ) =>
    unit;
};