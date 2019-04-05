open WebapiExtra;

module Base64Js = {
  [@bs.module "base64-js"] external _byteLength: string => int = "";
  [@bs.module "base64-js"]
  external toByteArray: string => Js.Typed_array.Uint8Array.t = "";
  [@bs.module "base64-js"]
  external fromByteArray: Js.Typed_array.Uint8Array.t => string = "";
};

type bytes = Js.Typed_array.array_buffer;

let arrayBufferToBase64 = buffer =>
  Base64Js.fromByteArray(Js.Typed_array.Uint8Array.fromBuffer(buffer));

let base64ToArrayBuffer = base64string =>
  Base64Js.toByteArray(base64string)->Js.Typed_array.Uint8Array.buffer;

let stringToArrayBuffer = str =>
  Dom.TextEncoder.create()
  ->Dom.TextEncoder.encode(str)
  ->Js.Typed_array.Uint8Array.buffer;

let arrayBufferToString = buffer =>
  Dom.TextDecoder.create()->Dom.TextDecoder.decode(buffer);