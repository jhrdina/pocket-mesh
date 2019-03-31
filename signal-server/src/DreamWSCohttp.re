module WS = Websocket.IO(Cohttp_lwt_unix.IO);

open Lwt.Infix;

module Socket = {
  type t = {
    fileDescr: Cohttp.Connection.t,
    framesOutFn: Websocket.Frame.t => Lwt.t(unit),
    onMessage: (string, string) => unit,
    onDisconnect: unit => unit,
  };
  let compare = (a, b) =>
    Cohttp.Connection.compare(a.fileDescr, b.fileDescr);
  let emit = (socket, msg) =>
    socket.framesOutFn(Websocket.Frame.create(~content=msg, ()));
  let setOnMessage = (f, socket) => {...socket, onMessage: f};
  let setOnDisconnect = (f, socket) => {...socket, onDisconnect: f};
  let create = (fileDescr, framesOutFn) => {
    fileDescr,
    framesOutFn,
    onMessage: (_, _) => (),
    onDisconnect: () => (),
  };
};

type tlsConfig = {
  cert: string,
  key: string,
};

module Server = {
  let sendFrames = (stream, oc) => {
    let buf = Buffer.create(128);
    let sendFrame = fr => {
      Buffer.clear(buf);
      WS.write_frame_to_buf(~mode=Server, buf, fr);
      Lwt_io.write(oc) @@ Buffer.contents(buf);
    };
    Lwt_stream.iter_s(sendFrame, stream);
  };
  let readFrames = (ic, oc, handlerFn) => {
    let readFrame = WS.make_read_frame(~mode=Server, ic, oc);
    let rec inner = () => readFrame() >>= Lwt.wrap1(handlerFn) >>= inner;
    inner();
  };
  let upgradeConnection = (request, incomingHandler) => {
    let headers = Cohttp.Request.headers(request);
    switch (Cohttp.Header.get(headers, "sec-websocket-key")) {
    | None => Lwt.fail_with("Missing sec-websocket-key header")
    | Some(key) =>
      let hash =
        key ++ Websocket.websocket_uuid |> Websocket.b64_encoded_sha1sum;
      let responseHeaders =
        Cohttp.Header.of_list([
          ("Upgrade", "websocket"),
          ("Connection", "Upgrade"),
          ("Sec-WebSocket-Accept", hash),
        ]);
      let resp =
        Cohttp.Response.make(
          ~status=`Switching_protocols,
          ~encoding=Cohttp.Transfer.Unknown,
          ~headers=responseHeaders,
          ~flush=true,
          (),
        );
      let (framesOutStream, framesOutFn) = Lwt_stream.create();
      let f = (ic, oc) =>
        Lwt.join([
          /* input: data from the client is read from the input channel
           * of the tcp connection; pass it to handler function */
          readFrames(ic, oc, incomingHandler),
          /* output: data for the client is written to the output
           * channel of the tcp connection */
          sendFrames(framesOutStream, oc),
        ]);
      Lwt.return((`Expert((resp, f)), framesOutFn));
    };
  };
  let httpHandler =
      (
        onConnection: Socket.t => Socket.t,
        conn: (Conduit_lwt_unix.flow, Cohttp.Connection.t),
        req: Cohttp_lwt_unix.Request.t,
        body: Cohttp_lwt.Body.t,
      ) => {
    let socket = ref(None);

    /*Lwt_io.eprintf(
        "[HTTP REQ] %s\n%!",
        Cohttp.Connection.to_string @@ snd(conn)
      )
      >>= ((_) => */ Cohttp_lwt.Body.drain_body(
      body,
    )  /*)*/
    >>= (
      () =>
        upgradeConnection(req, f =>
          switch (f.opcode) {
          | Websocket.Frame.Opcode.Close =>
            switch (socket^) {
            | Some(s) => s.Socket.onDisconnect()
            | None =>
              Printf.eprintf("Weird, disconected before onConnection...")
            }
          | _ =>
            switch (socket^) {
            | Some(s) => s.onMessage("", f.content)
            | None =>
              Printf.eprintf(
                "Weird, receiving message before onConnection...",
              )
            }
          }
        )
    )
    >>= (
      ((resp, framesOutFn)) => {
        let newSocket =
          Socket.create(snd(conn), Lwt.wrap1(f => framesOutFn(Some(f))));
        socket := Some(onConnection(newSocket));
        Lwt.return(resp);
      }
    );
  };
  let onServerConnectionClosed = ((ch, _)) =>
    Printf.eprintf(
      "[SERV] connection %s closed\n%!",
      Sexplib.Sexp.to_string_hum(Conduit_lwt_unix.sexp_of_flow(ch)),
    );
  let startServer = (~port, ~tls, ~onConnection) =>
    Lwt_io.eprintf("[SERV] Listening on port %d\n%!", port)
    >>= (
      () => {
        let mode =
          switch (tls) {
          | None =>
            Printf.printf("TCP mode started\n%!");
            `TCP(`Port(port));
          | Some({cert, key}) =>
            Printf.printf("TLS mode started\n%!");
            `TLS((
              `Crt_file_path(cert),
              `Key_file_path(key),
              `No_password,
              `Port(port),
            ));
          };
        Cohttp_lwt_unix.Server.create(
          ~mode,
          Cohttp_lwt_unix.Server.make_response_action(
            ~callback=httpHandler(onConnection),
            ~conn_closed=onServerConnectionClosed,
            (),
          ),
        );
      }
    );
};

let run = (~port, ~tls=None, ~onConnection, ()) =>
  Lwt_main.run(Server.startServer(~port, ~tls, ~onConnection));