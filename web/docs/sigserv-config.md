---
id: sigserv-config
title: Configuration and Running
---

Signal server, once compiled, needs only a few dependencies and doesn't require a lot of system resources.

## Runtime dependencies

To run the signal server, the following libraries are needed _(corresponding packages for Ubuntu 18.04 are in the parentheses)_:

- GMP (`libgmp10`)
- OpenSSL (`libssl1.1`)

In Ubuntu you can install them using a single command:

```sh
sudo apt install libgmp10 libssl1.1
```

## Running

You can run the server binary with default settings (port 7777, TLS disabled) using

```sh
./signal-server
```

The server becomes available on `ws://<domain>:7777`, eg. `ws://localhost:7777`.

You can change the default **port** using

```sh
./signal-server <port>
```

To run in **TLS mode**, the command looks like

```sh
./signal-server --cert "path/to/certificate.pem" --key "path/to/private-key.pem" 443
```

The final address of your server will be `wss://<domain>`. There is no need to specify port because 443 is default for HTTPS/WSS.