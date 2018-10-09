open BlackTea;
open Js.Promise;
let wrapPromise = (runPromise, successToMsg, errorToMsg) =>
  Cmd.call(callbacks =>
    runPromise()
    |> then_(value => callbacks^.enqueue(successToMsg(value)) |> resolve)
    |> catch(exn => callbacks^.enqueue(errorToMsg(exn)) |> resolve)
    |> ignore
  );
let wrapPairPromise = (runPromise, successToMsg, errorToMsg) =>
  Cmd.call(callbacks =>
    runPromise()
    |> then_(((a, b)) =>
         callbacks^.enqueue(successToMsg(a, b)) |> resolve
       )
    |> catch(exn => callbacks^.enqueue(errorToMsg(exn)) |> resolve)
    |> ignore
  );

let none = Cmd.none;
let batch = Cmd.batch;
let log = str => Cmd.call(_ => str |> Js.log);