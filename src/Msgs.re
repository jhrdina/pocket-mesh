type t = ..;
type t +=
  | CryptoFatalError(exn)
  /* ====== */
  /* Global */
  /* ====== */
  /* Others */
  | RemoveThisPeerAndAllData
  /* ===== */
  /* Debug */
  /* ===== */
  | SendToPeer(string, string)
  | Noop;

let cryptoFatalError = exn => CryptoFatalError(exn);
let removeThisPeerAndAllData = RemoveThisPeerAndAllData;
let sendToPeer = (a, b) => SendToPeer(a, b);
let noop = Noop;