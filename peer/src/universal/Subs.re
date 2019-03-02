open BlackTea;

let ofStream = (key, streamCreator) => {
  // BlackTea.Sub.registration(key, enableCall);
  let enableCall: (VdomRe.applicationCallbacks('a), unit) => unit =
    callbacks => {
      let streamSub =
        streamCreator() |> Wonka.subscribe((. msg) => callbacks.enqueue(msg));
      streamSub.unsubscribe;
    };
  Sub.registration(key, enableCall);
};

let timeout = (key, msTime, tagger) => {
  open VdomRe;
  let enableCall = callbacks => {
    let id =
      Js.Global.setTimeout(
        // () => callbacks.enqueue(tagger(Js.Date.now())),
        () => callbacks.enqueue(tagger),
        msTime,
      );
    () => Js.Global.clearTimeout(id);
  };
  Sub.registration(key, enableCall);
};

let interval = (~key, interval, tagger) => {
  open VdomRe;
  let enableCall = callbacks => {
    let id =
      Js.Global.setInterval(() => callbacks.enqueue(tagger), interval);
    () => Js.Global.clearInterval(id);
  };
  Sub.registration(key, enableCall);
};