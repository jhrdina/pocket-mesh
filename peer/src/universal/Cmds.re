open BlackTea;
open Js.Promise;

let fromPromise = (runPromise, completedToMsg) =>
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

let none = Cmd.none;
let batch = Cmd.batch;
let log = str => Cmd.call(_ => str |> Js.log);

let timeout = (itIsTimeMsg, delayMs) =>
  /* TODO: Cancel support? */
  Cmd.call(callbacks =>
    Js.Global.setTimeout(() => callbacks^.enqueue(itIsTimeMsg), delayMs)
    |> ignore
  );