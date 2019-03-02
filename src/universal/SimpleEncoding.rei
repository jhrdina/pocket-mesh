/* String <-> Bytes <-> Base64 */
type bytes = Js.Typed_array.array_buffer;

let arrayBufferToBase64: bytes => string;
let base64ToArrayBuffer: string => bytes;
let stringToArrayBuffer: string => bytes;
let arrayBufferToString: bytes => string;