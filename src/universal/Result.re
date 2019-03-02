[@bs.deriving accessors]
type t('a, 'b) =
  | Ok('a)
  | Error('b);

let mapOk = (f, t) =>
  switch (t) {
  | Ok(x) => Ok(f(x))
  | Error(x) => Error(x)
  };

let bind = (f, t) =>
  switch (t) {
  | Ok(x) => f(x)
  | Error(x) => Error(x)
  };