# Pocket Mesh Signal Server

Native websockets-based signal server for Pocket Mesh framework written in ReasonML.

**Features**:

- Keep track of connected peers and their IDs (fingerprints of their public keys)
- Notify on changes of selected peers state (going online/offline)
- Forwards SDP and key-exchange messages for WebRTC connection establishment
- TLS support
- [not implemented] Verification of connected peers using signatures

[`Reason`](http://reasonml.github.io/) project for native compilation:

[More info on the workflow](https://reasonml.github.io/guide/native).

## Develop With OPAM

**Build**:

Clone the repo and run these commands from within the project:

```sh
opam update # get the latest opam packages data. Skip this optionally
# opam will read into the `opam` file and add the other dependencies
opam switch 4.06.1 # recommended OCaml compiler version
opam install reason
opam install merlin
opam install re
opam install websocket-lwt
make build    # build/rebuild your files
```

**Run**:

```sh
make run
```

or

```sh
./_build/install/default/bin/signal-server
```

**Develop**:

- Make sure you have `merlin` globally installed (via opam, _not_
  `reason-cli`!)
- [See the ReasonML
  docs](https://reasonml.github.io/docs/en/editor-plugins.html) about setting
  up your editor. Just remember to _not_ install `reason-cli` when using
  `opam`.

**Develop**:

- [See the ReasonML
  docs](https://reasonml.github.io/docs/en/editor-plugins.html) about setting
  up your editor.
- Start your editor from the root of this project via `esy vim` or `esy atom`
  etc. (Note VSCode has special `esy` support so that you don't need to start
  it this way from the command line).
- Add dependencies by adding entries to the `package.json`, running `esy install` then `esy build`.

## Developing Your Project

The entrypoint of this project is the `./bin/test.re` file. Make a simple
change to it and then rerun the build.

`ReasonNativeProject` is meant to be the starting point of your own project. You'll
want to make use of existing libraries in your app, so browse the growing set
of `opam` packages in the [opam repository](http://opam.ocaml.org/packages/).

## Troubleshooting

In general, if something goes wrong, try upgrading your install of the project
by running `opam upgrade ReasonNativeProject`, or if it failed to install and you
later fixed it, `opam install ReasonNativeProject`.
