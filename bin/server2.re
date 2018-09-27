Server.run(
  ~port=9876,
  ~onMessage=(text, reply) => {
    reply("Dikec za " ++ text)
  },
  ~httpFallback=(method, path, headers, msg, respond) => {
    respond(
          "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Type: text/plain\r\n\r\nHello",
    )
  },
  ~config=(module Server.UnixConfig: Server.Config)
)