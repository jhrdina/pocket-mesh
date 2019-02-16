open BlackTea;
open Js.Promise;
let wrapPromise = (runPromise, successToMsg, exnToMsg) =>
  Cmd.call(callbacks =>
    runPromise()
    |> then_(value => callbacks^.enqueue(successToMsg(value)) |> resolve)
    |> catch(error =>
         callbacks^.enqueue(error |> JsUtils.promiseErrorToExn |> exnToMsg)
         |> resolve
       )
    |> ignore
  );

let wrapResPromise = (runPromise, completedToMsg) =>
  Cmd.call(callbacks =>
    runPromise()
    |> then_(value =>
         callbacks^.enqueue(completedToMsg(Result.Ok(value))) |> resolve
       )
    |> catch(error =>
         callbacks^.enqueue(
           completedToMsg(Error(error |> JsUtils.promiseErrorToExn)),
         )
         |> resolve
       )
    |> ignore
  );

let wrapPairPromise = (runPromise, successToMsg, exnToMsg) =>
  Cmd.call(callbacks =>
    runPromise()
    |> then_(((a, b)) =>
         callbacks^.enqueue(successToMsg(a, b)) |> resolve
       )
    |> catch(error =>
         callbacks^.enqueue(error |> JsUtils.promiseErrorToExn |> exnToMsg)
         |> resolve
       )
    |> ignore
  );

let none = Cmd.none;
let batch = Cmd.batch;
let log = str => Cmd.call(_ => str |> Js.log);

let timeout = (itIsTimeMsg, delayMs) =>
  /* TODO: Cancel support? */
  Cmd.call(callbacks =>
    Js.Global.setTimeout(() => callbacks^.enqueue(itIsTimeMsg), delayMs)
    |> ignore
  );