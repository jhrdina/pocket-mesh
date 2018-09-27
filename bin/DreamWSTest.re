DreamWS.run(
  ~port=7777,
  ~onConnection=socket => {
    open DreamWS;
    ignore(Socket.emit(socket, "asdf"));
    socket
    |> Socket.setOnMessage((from, msg) => {
      Printf.eprintf("Message: %s\n%!", msg);
      ignore(Socket.emit(socket, "Díkec za " ++ msg));
    })
    |> Socket.setOnDisconnect(() => Printf.eprintf("Disconnected\n%!"));
  }
);