let thenl = cb =>
  Js.Promise.then_(v => {
    Js.log(v);
    cb(v);
  });

let promiseErrorToExn: Js.Promise.error => exn =
  x => Caml_js_exceptions.internalToOCamlException(Obj.magic(x));

let emptyPromise = () =>
  Js.Promise.make((~resolve, ~reject) => {
    let n = ();
    resolve(. n);
  });