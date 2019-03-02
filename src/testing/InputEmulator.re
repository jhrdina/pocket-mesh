type t = {
  mutable counter: int,
  mutable timeoutIdOpt: option(Js.Global.timeoutId),
};

let randomIntFromTo = (min, max) => Random.int(max - min + 1) + min;

let start = (prefix, cb, t) => {
  let randomDelay = () =>
    if (Random.int(8 * 7) > 1) {
      randomIntFromTo(130, 350);
    } else {
      /* Break */
      randomIntFromTo(1500, 4000);
    };

  let rec planNext = () => {
    switch (t.timeoutIdOpt) {
    | Some(timeoutId) => Js.Global.clearTimeout(timeoutId)
    | None => ()
    };

    t.timeoutIdOpt =
      Some(
        Js.Global.setTimeout(
          () => {
            t.counter = t.counter + 1;
            cb(prefix ++ "_" ++ string_of_int(t.counter));
            planNext();
          },
          randomDelay(),
        ),
      );
  };
  planNext();
};

let stop = t =>
  switch (t.timeoutIdOpt) {
  | Some(timeoutId) =>
    Js.Global.clearTimeout(timeoutId);
    t.timeoutIdOpt = None;
  | None => ()
  };

let create = () => {counter: 0, timeoutIdOpt: None};

module Cmds = {
  open BlackTea;

  let start = (cbToMsg, prefix, t) =>
    Cmd.call(callbacks =>
      start(prefix, str => str |> cbToMsg |> callbacks^.enqueue, t)
    );

  let stop = t => Cmd.call(_callbacks => stop(t));

  let create = emulatorToMsg =>
    Cmd.call(callbacks => create() |> emulatorToMsg |> callbacks^.enqueue);
};