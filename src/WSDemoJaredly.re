Reason_websocket.Server.run(
  ~port=7777,
  ~onMessage=(text, reply) => {
    reply("Díkec z normálního za " ++ text);
  },
  ~httpFallback=(method, path, headers, msg, respond) => {
    respond(
          "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Type: text/plain\r\n\r\nHello"
    );
  },
  ~config=(module Reason_websocket.Server.UnixConfig: Reason_websocket.Server.Config)
);