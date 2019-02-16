let ofStream = (key, streamCreator) => {
  // BlackTea.Sub.registration(key, enableCall);
  let enableCall: (VdomRe.applicationCallbacks('a), unit) => unit =
    callbacks => {
      let streamSub =
        streamCreator() |> Wonka.subscribe((. msg) => callbacks.enqueue(msg));
      streamSub.unsubscribe;
    };
  BlackTea.Sub.registration(key, enableCall);
};