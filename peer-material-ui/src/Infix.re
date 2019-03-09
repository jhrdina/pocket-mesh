/**
 * This module is provided for easier working with optional values.
 */

/** The "force unwrap" operator
   *
   * If you're sure there's a value, you can force it.
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |! "Expected this to be present";
   * Js.log(x);
   * ```
   *
   * But you gotta be sure, otherwise it will throw.
   * ```reason;raises
   * open Json.Infix;
   * let x: int = None |! "This will throw";
   * ```
   */
let (|!) = (o, d) =>
  switch (o) {
  | None => failwith(d)
  | Some(v) => v
  };
/** The "upwrap with default" operator
   * ```
   * open Json.Infix;
   * let x: int = Some(10) |? 4;
   * let y: int = None |? 5;
   * Js.log2(x, y);
   * ```
   */
let (|?) = (o, d) =>
  switch (o) {
  | None => d
  | Some(v) => v
  };
/** The "transform contents into new optional" operator
   * ```
   * open Json.Infix;
   * let maybeInc = x => x > 5 ? Some(x + 1) : None;
   * let x: option(int) = Some(14) |?> maybeInc;
   * let y: option(int) = None |?> maybeInc;
   * ```
   */
let (|?>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => fn(v)
  };
/** The "transform contents into new value & then re-wrap" operator
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: option(int) = Some(7) |?>> inc;
   * let y: option(int) = None |?>> inc;
   * Js.log2(x, y);
   * ```
   */
let (|?>>) = (o, fn) =>
  switch (o) {
  | None => None
  | Some(v) => Some(fn(v))
  };
/** "handle the value if present, otherwise here's the default"
   *
   * It's called fold because that's what people call it :?. It's the same as "transform contents to new value" + "unwrap with default".
   *
   * ```
   * open Json.Infix;
   * let inc = x => x + 1;
   * let x: int = fold(Some(4), 10, inc);
   * let y: int = fold(None, 2, inc);
   * Js.log2(x, y);
   * ```
   */
let fold = (o, d, f) =>
  switch (o) {
  | None => d
  | Some(v) => f(v)
  };