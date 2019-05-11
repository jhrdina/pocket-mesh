/* This file was taken from https://github.com/jaredly/reason-websocket */
/*
* Copyright (c) 2018 Jared Forsyth <jared@jaredforsyth.com>
*
* Permission to use, copy, modify, and distribute this software for any
* purpose with or without fee is hereby granted, provided that the above
* copyright notice and this permission notice appear in all copies.
*
* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

module type Config = {
  type sock;
  let read: (sock, int) => option(string);
  let write: (sock, string) => unit;
  let acceptSocket: Unix.file_descr => sock;
  let init: unit => unit;
};

module Connection = (Config: Config) => {
  type t('a) = ('a => unit) => unit;
  let return = (x, fin) => fin(x);
  let bind = (work, use, fin) => work(result => (use(result))(fin));
  let (>>=) = bind;
  type ic = Config.sock;
  type oc = Config.sock;
  type conn;
  let read = (sock, size, fin) =>
    fin(
      switch (Config.read(sock, size)) {
      | None => ""
      | Some(s) => s
      },
    );
  let write = (sock, text, fin) => {
    Config.write(sock, text);
    fin();
  };
};

module Server = (Config: Config) => {
  module WSock = Websocket.IO((Connection(Config)));
  let handleWebsocket = (~onMessage, _path, headers, socket) => {
    let key =
      Http.StringMap.find("Sec-WebSocket-Key", headers) |> String.trim;
    let response = Websocket.make_handshake_response(key);
    Config.write(socket, response);

    let loop = ref(true);
    let buffer = Buffer.create(1024);
    while (loop^) {
      /* TODO: Catch End_of_file or any other exception here and you can handle disconnecting.
          As default exceptions are not catched and therefore simply kill the thread with current connection.
         */
      WSock.make_read_frame(~mode=WSock.Server, socket, socket, (), frame =>
        onMessage(
          frame.Websocket.Frame.content,
          response => {
            Buffer.clear(buffer);
            WSock.write_frame_to_buf(
              ~mode=WSock.Server,
              buffer,
              Websocket.Frame.create(~content=response, ()),
            );
            Config.write(socket, Buffer.contents(buffer));
          },
        )
      );
    };
  };
  let handleConnection = (~onMessage, ~httpFallback, socket) => {
    let loop = ref(true);
    while (loop^) {
      switch (Config.read(socket, 1024)) {
      | None => loop := false
      | Some(msg) =>
        let (method, path, headers) = Http.parse_request(msg);
        let shouldUpgrade =
          Http.StringMap.exists(
            (k, v) => k == "Upgrade" && String.trim(v) == "websocket",
            headers,
          );
        if (shouldUpgrade) {
          handleWebsocket(~onMessage, path, headers, socket);
        } else {
          httpFallback(method, path, headers, msg, response =>
            Config.write(socket, response)
          );
        };
      };
    };
  };
};

module UnixConfig = {
  type sock = Unix.file_descr;
  let read = (sock, maxlen) => {
    let bytes = Bytes.create(maxlen);
    let len = Unix.recv(sock, bytes, 0, maxlen, []);
    Some(Bytes.sub_string(bytes, 0, len));
  };
  let write = (sock, text) => {
    let bytes = Bytes.of_string(text);
    let total = Bytes.length(bytes);
    let left = ref(Bytes.length(bytes));
    while (left^ > 0) {
      left := left^ - Unix.send(sock, bytes, total - left^, left^, []);
    };
  };
  let init = () => ();
  let acceptSocket = socket => socket;
};

let run = (~port, ~onMessage, ~httpFallback, ~config: (module Config)) => {
  module Config = (val config);
  Config.init();
  module S = Server(Config);
  /* Set up socket */
  let listeningSocket = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
  Unix.setsockopt(listeningSocket, Unix.SO_REUSEADDR, true);
  Unix.bind(listeningSocket, Unix.ADDR_INET(Unix.inet_addr_any, port));
  Unix.listen(listeningSocket, 100);
  print_endline(Printf.sprintf("Listening on port %d...", port));
  while (true) {
    let (clientSocket, _clientAddr) = Unix.accept(listeningSocket);
    let sock = Config.acceptSocket(clientSocket);
    ignore(
      Thread.create(S.handleConnection(~onMessage, ~httpFallback), sock),
    );
  };
};