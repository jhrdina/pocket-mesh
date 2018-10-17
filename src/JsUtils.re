let thenl = cb =>
  Js.Promise.then_(v => {
    Js.log(v);
    cb(v);
  });

let promiseErrorToExn: Js.Promise.error => exn =
  x => Js.Exn.internalToOCamlException(Obj.magic(x));