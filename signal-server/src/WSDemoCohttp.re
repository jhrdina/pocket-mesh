// This file was taken from https://github.com/vbmithr/ocaml-websocket/blob/master/test/upgrade_connection.ml
/*
 * Copyright (c) 2016 Vincent Bernardoff
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH
 * REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, 
 * INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE.
 */

open Lwt;

open Websocket_cohttp_lwt;

let handler =
    (
      conn: (Conduit_lwt_unix.flow, Cohttp.Connection.t),
      req: Cohttp_lwt_unix.Request.t,
      body: Cohttp_lwt.Body.t
    ) =>
  Frame.(
    Lwt_io.eprintf("[CONN] %s\n%!", Cohttp.Connection.to_string @@ snd(conn))
    >>= (
      (_) => {
        let uri = Cohttp.Request.uri(req);
        switch (Uri.path(uri)) {
        | "/" =>
          Lwt_io.eprintf("[PATH] /\n%!")
          >>= (
            () =>
              Cohttp_lwt_unix.Server.respond_string(
                ~status=`OK,
                ~body=
                  {|
        <html>
        <head>
          <meta charset="utf-8">
          <script>
            function sendshit() {
              ws.send(document.querySelector('#input').value);
            }

            document.addEventListener("DOMContentLoaded", () => {
              window.ws = new WebSocket('ws://localhost:7777/ws');
              ws.onmessage = (x) => {
                console.log(x.data);
                sendshit();
              };
            });
          </script>
        </head>
        <body>
            <input id="input">
            <button onclick="sendshit()">Send</button>
            <div id='msg'></div>
        </body>
        </html>
        |},
                ()
              )
          )
        | "/ws" =>
          Lwt_io.eprintf("[PATH] /ws\n%!")
          >>= (() => Cohttp_lwt.Body.drain_body(body))
          >>= (
            () =>
              Websocket_cohttp_lwt.upgrade_connection(req, fst(conn), f =>
                switch f.opcode {
                | Opcode.Close => Printf.eprintf("[RECV] CLOSE\n%!")
                | _ => Printf.eprintf("[RECV] %s\n%!", f.content)
                }
              )
          )
          >>= (
            ((resp, body, frames_out_fn)) => {
              let msg = "Zdarec kliente +ěščřžýáí";
              ignore(
                Lwt_io.eprintf("[SEND] %s\n%!", msg)
                >>= (
                  () =>
                    Lwt.wrap1(frames_out_fn) @@
                    Some(Frame.create(~content=msg, ()))
                )
              );
              Lwt.return((resp, (body :> Cohttp_lwt.Body.t)));
            }
          )
        | _ =>
          Lwt_io.eprintf("[PATH] Catch-all\n%!")
          >>= (
            () =>
              Cohttp_lwt_unix.Server.respond_string(
                ~status=`Not_found,
                ~body=
                  Sexplib.Sexp.to_string_hum(Cohttp.Request.sexp_of_t(req)),
                ()
              )
          )
        };
      }
    )
  );

let start_server = port => {
  let conn_closed = ((ch, _)) =>
    Printf.eprintf(
      "[SERV] connection %s closed\n%!",
      Sexplib.Sexp.to_string_hum(Conduit_lwt_unix.sexp_of_flow(ch))
    );
  Lwt_io.eprintf("[SERV] Listening for HTTP on port %d\n%!", port)
  >>= (
    () =>
      Cohttp_lwt_unix.Server.create(
        ~mode=`TCP(`Port(port)),
        Cohttp_lwt_unix.Server.make(~callback=handler, ~conn_closed, ())
      )
  );
};

let () = Lwt_main.run(start_server(7777));