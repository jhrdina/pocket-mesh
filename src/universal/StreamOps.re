open Wonka_types;
open Wonka_helpers;

let fromPromise: (Js.Promise.t('a), sinkT(Result.t('a, exn))) => unit =
  promise =>
    curry(sink => {
      let ended = ref(false);

      ignore(
        promise
        |> Js.Promise.then_(value => {
             if (! ended^) {
               sink(. Push(value |> Result.ok));
               sink(. End);
             };
             Js.Promise.resolve();
           })
        |> Js.Promise.catch(err => {
             if (! ended^) {
               sink(.
                 Push(err |> JsUtils.promiseErrorToExn |> Result.error),
               );
               sink(. End);
             };
             Js.Promise.resolve();
           }),
      );

      sink(.
        Start(
          (. signal) =>
            switch (signal) {
            | Close => ended := true
            | _ => ()
            },
        ),
      );
    });

let filterMap: ((. 'a) => option('b), sourceT('a), sinkT('b)) => unit =
  f =>
    curry(source =>
      curry(sink =>
        captureTalkback(source, (. signal, talkback) =>
          switch (signal) {
          | Start(x) => sink(. Start(x))
          | Push(x) =>
            switch (f(. x)) {
            | Some(y) => sink(. Push(y))
            | None => talkback(. Pull)
            }
          | End => sink(. End)
          }
        )
      )
    );

type retryWhenStateT = {
  mutable failedAttempts: int,
  mutable talkback: (. talkbackT) => unit,
  mutable gotCloseSignal: bool,
  mutable pullPending: bool,
  mutable timeoutId: option(Js.Global.timeoutId),
};

// type retryWhenStateT =
//   | WaitingForRetry(tb, failedAttempts, pullPending, timeoutId)
//   | WaitingForInput(tb, failedAttempts, pullPending)
//   | Closed;

let retryWhen = f =>
  curry(source =>
    curry(sink => {
      let state: retryWhenStateT = {
        failedAttempts: 0,
        talkback: Wonka_helpers.talkbackPlaceholder,
        gotCloseSignal: false,
        pullPending: false,
        timeoutId: None,
      };

      let rec subscribe = () => {
        source((. signal) =>
          switch (signal) {
          | Start(tb) =>
            state.talkback = tb;
            if (state.pullPending) {
              state.talkback(. Pull);
            };

          | Push(Some(x)) when !state.gotCloseSignal =>
            state.failedAttempts = 0;
            switch (state.timeoutId) {
            | Some(timeoutId) =>
              Js.Global.clearTimeout(timeoutId);
              state.timeoutId = None;
            | None => ()
            };
            state.pullPending = false;
            sink(. Push(Some(x)));

          | Push(None) when !state.gotCloseSignal && state.timeoutId == None =>
            state.failedAttempts = state.failedAttempts + 1;
            state.timeoutId =
              Some(
                Js.Global.setTimeout(
                  () => {
                    state.timeoutId = None;
                    state.talkback(. Close);
                    if (!state.gotCloseSignal) {
                      subscribe();
                    };
                  },
                  f(state.failedAttempts),
                ),
              );
            sink(. Push(None));

          | End when !state.gotCloseSignal && state.timeoutId == None =>
            sink(. End)

          | Push(_)
          | End => ()
          }
        );
      };
      subscribe();

      sink(.
        Start(
          (. signal) => {
            switch (signal) {
            | Close => state.gotCloseSignal = true
            | Pull => state.pullPending = true
            };
            state.talkback(. signal);
          },
        ),
      );
    })
  );