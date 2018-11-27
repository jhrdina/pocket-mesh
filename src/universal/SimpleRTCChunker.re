open Rex_json;
open Json.Infix;

/* P2P messages chunking and reassambling */

/* CONSTANTS */

let maxUtfCharSize = 4;
/* Not 65536 because of bug in Firefox versions <57, 60) */
let maxSupportedMessageSize = 65535;
let maxP2PStringLength = maxSupportedMessageSize / maxUtfCharSize;

/* TYPES */

type headerPayloadType =
  | String
  | Binary;

type payload =
  | String(string)
  | ArrayBuffer(Js.Typed_array.ArrayBuffer.t);

type hdr = {
  payloadType: headerPayloadType,
  payloadLength: int,
};

type t =
  | Idle
  /* receivedData, payloadType, receivedSize */
  | Receiving(Js.Typed_array.ArrayBuffer.t, headerPayloadType, int);

/* MODULES */

module Header: {
  open Belt;
  type t;
  type error =
    | TooLong;

  /* let make: (string, payload) => Result.t(t, error); */
  let ofString: string => Result.t(t, error);
  let toString: t => string;
} = {
  open Belt;
  type t = string;
  type error =
    | TooLong;
  let ofString = s =>
    String.length(s) <= maxP2PStringLength ? Result.Ok(s) : Error(TooLong);
  let toString = t => t;
};

/* ENCODERS/DECODERS */

let encodePayloadType: headerPayloadType => string =
  fun
  | String => "string"
  | Binary => "binary";

let decodePayloadType: string => option(headerPayloadType) =
  fun
  | "string" => Some(String)
  | "binary" => Some(Binary)
  | _ => None;

let encodeHdr = t =>
  Json.(
    Object([
      ("payloadType", Json.String(t.payloadType |> encodePayloadType)),
      ("payloadLength", Number(float_of_int(t.payloadLength))),
    ])
  );

let decodeHdr = json =>
  Json.(
    switch (
      json |> get("payloadType") |?> string |?> decodePayloadType,
      json |> get("payloadLength") |?> number |?>> int_of_float,
    ) {
    | (Some(payloadType), Some(payloadLength)) =>
      Some({payloadType, payloadLength})
    | _ => None
    }
  );

/* HELPERS */

let iterArrayBufferByChunks = (f, chunkSize, buffer) => {
  let rec send = (start, buffer) => {
    open Js.Typed_array;
    let end_ = min(start + chunkSize, buffer |> ArrayBuffer.byteLength);
    let chunk = buffer |> ArrayBuffer.slice(~start, ~end_);

    f(chunk);

    if (end_ < ArrayBuffer.byteLength(buffer)) {
      send(end_, buffer);
    };
  };
  send(0, buffer);
};

[@bs.send.pipe: Js.Typed_array.Uint8Array.t]
external setUint8ArrayAt: (Js.Typed_array.Uint8Array.t, int) => unit = "set";
let setArrayBufferAt = (buf, offset, t) =>
  Js.Typed_array.(
    setUint8ArrayAt(
      Uint8Array.fromBuffer(buf),
      offset,
      Uint8Array.fromBuffer(t),
    )
  );

let payloadOfArrayBuffer = (headerPayloadType: headerPayloadType, buf) =>
  switch (headerPayloadType) {
  | String => String(buf |> SimpleEncoding.arrayBufferToString)
  | Binary => ArrayBuffer(buf)
  };

/* MAIN FUNCTIONS */

let make = () => Idle;

let recv = (data, t) =>
  switch (data, t) {
  | (SimpleRTC.String(str), Idle | Receiving(_)) =>
    str
    |> Json.parseOpt
    |?> decodeHdr
    |?>> (
      hdr => {
        let payload = Js.Typed_array.ArrayBuffer.make(hdr.payloadLength);
        hdr.payloadLength > 1 ?
          (Receiving(payload, hdr.payloadType, 0), None) :
          /* There will be no following payload messages. */
          (Idle, Some(payloadOfArrayBuffer(hdr.payloadType, payload)));
      }
    )
    |? (t, None)

  | (
      ArrayBuffer(newData),
      Receiving(receivedData, payloadType, receivedSize),
    ) =>
    open Js.Typed_array;
    let readSize =
      min(
        ArrayBuffer.byteLength(newData),
        ArrayBuffer.byteLength(receivedData) - receivedSize,
      );
    receivedData |> setArrayBufferAt(newData, receivedSize);
    let newReceivedSize = receivedSize + readSize;
    if (newReceivedSize >= ArrayBuffer.byteLength(receivedData)) {
      (Idle, Some(payloadOfArrayBuffer(payloadType, receivedData)));
    } else {
      (Receiving(receivedData, payloadType, newReceivedSize), None);
    };

  | (ArrayBuffer(_), Idle) => (t, None)
  };

/* TODO: Add user headers support */
let send = (simpleRtc, payload) => {
  let (payloadType: headerPayloadType, payloadBuf) =
    switch (payload) {
    | String(str) => (String, str |> SimpleEncoding.stringToArrayBuffer)
    | ArrayBuffer(buf) => (Binary, buf)
    };

  simpleRtc->SimpleRTC.send(
    String(
      {
        payloadType,
        payloadLength: payloadBuf |> Js.Typed_array.ArrayBuffer.byteLength,
      }
      |> encodeHdr
      |> Json.stringify,
    ),
  );

  payloadBuf
  |> iterArrayBufferByChunks(
       chunk => simpleRtc->SimpleRTC.send(ArrayBuffer(chunk)),
       maxSupportedMessageSize,
     );
};