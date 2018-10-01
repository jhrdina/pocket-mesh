let thenl = cb =>
  Js.Promise.then_(v => {
    Js.log(v);
    cb(v);
  });
();