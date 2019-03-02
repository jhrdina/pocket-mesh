open BlackTea;

/*
 EXAMPLE USAGE

 [@bs.set_index]
 external makeGlobal: (Webapi.Dom.Window.t, string, 'a) => unit = "";
 let makeGlobal = (name, value) => makeGlobal(Webapi.Dom.window, name, value);

 [@bs.deriving accessors]
 type msg =
   | FrequentMsg
   | DebouncedMsg
   | Deb(Debouncer.msg(msg));

 type t = {debouncer: Debouncer.t(mmsg)};

 let stup =
   BlackTea.Store.create(
     ~init=() => ({debouncer: Debouncer.init()}, Cmd.none),
     ~update=
       (model, msg) =>
         switch (msg) {
         | FrequentMsg => (
             model,
             Debouncer.debounceCmd(debouncedMsg, deb, 4000),
           )
         | DebouncedMsg => (model, Cmds.log("Jepeee"))
         | Deb(msg) =>
           Debouncer.update(d => {debouncer: d}, model.debouncer, msg, deb)
         },
     ~subscriptions=_model => Sub.none,
     ~shutdown=_model => Cmds.none,
   );

 makeGlobal("push", () => stup.pushMsg(FrequentMsg));

 /* Open console and try calling push() repeatedly */

 */

/* TYPES */

type t('msg) =
  | Idle
  | ReheatingWithoutBullet
  | ReheatingWithBullet('msg, int);

type msg('msg) =
  | NewRequest('msg, int)
  | Timeout;

let init = () => Idle;

let update = (updater, model, msg, internalToMsg) =>
  switch (msg, model) {
  | (NewRequest(finMsg, time), Idle)
  | (Timeout, ReheatingWithBullet(finMsg, time)) => (
      ReheatingWithoutBullet |> updater,
      Cmd.batch([
        Cmd.msg(finMsg),
        Cmds.timeout(Timeout |> internalToMsg, time),
      ]),
    )
  | (
      NewRequest(finMsg, time),
      ReheatingWithoutBullet | ReheatingWithBullet(_),
    ) => (
      ReheatingWithBullet(finMsg, time) |> updater,
      Cmd.none,
    )
  | (Timeout, Idle)
  | (Timeout, ReheatingWithoutBullet) => (Idle |> updater, Cmd.none)
  };

let debounce = (succMsg, internalToMsg, time) =>
  NewRequest(succMsg, time) |> internalToMsg;

let debounceCmd = (succMsg, internalToMsg, time) =>
  Cmd.msg(debounce(succMsg, internalToMsg, time));