open Rex_json;
open Rex_json.Json.Infix;

let latestVersion = 1;

type offerOrAnswer = {
  src: string,
  tg: string,
  sdp: string,
  signature: string,
};

type login = {
  src: string,
  watch: list(string),
  signature: string,
};

type logoff = {
  src: string,
  signature: string,
};

type error =
  | TargetNotOnline
  | InvalidMessage(string);

type t =
  | Login(login)
  | Offer(offerOrAnswer)
  | Answer(offerOrAnswer)
  | Error(error)
  | Logoff(logoff)
  | Ok
  | Unknown;

type clientToServer = t;
type serverToClient = t;

let toJSON =
  fun
  | Offer(msg) as v
  | Answer(msg) as v => {
      let typeString =
        switch (v) {
        | Offer(_) => "offer"
        | _ => "answer"
        };
      Json.(
        stringify(
          Object([
            ("type", String(typeString)),
            ("src", String(msg.src)),
            ("tg", String(msg.tg)),
            ("sdp", String(msg.sdp)),
            ("signature", String(msg.signature)),
          ]),
        )
      );
    }
  | Error(error) =>
    Json.(
      stringify(
        Object([
          ("type", String("error")),
          ...switch (error) {
             | TargetNotOnline => [("code", String("TargetNotOnline"))]
             | InvalidMessage(explanation) => [
                 ("code", String("InvalidMessage")),
                 ("explanation", String(explanation)),
               ]
             },
        ]),
      )
    )
  | Ok => Json.(stringify(Object([("type", String("ok"))])))
  | _ => "";

type parsingResult('a) =
  | Ok('a)
  | Error(string);

/* f = i => i |> Json.string */
let decodeList = (f, json) =>
  Json.Infix.(
    json
    |> Json.array
    |?> (
      items =>
        List.fold_right(
          (itemJson, acc) =>
            switch (f(itemJson), acc) {
            | (Some(i), Some(list)) => Some([i, ...list])
            | _ => None
            },
          items,
          Some([]),
        )
    )
  );

let decodeLoginMsg = json =>
  switch (
    json |> Json.get("src") |?> Json.string,
    json |> Json.get("signature") |?> Json.string,
    json |> Json.get("watch") |?> decodeList(i => i |> Json.string),
  ) {
  | (Some(src), Some(signature), Some(watch)) =>
    Ok(Login({src, signature, watch}))
  | _ => Error("Login message invalid format")
  };

let decodeLogoffMsg = json =>
  switch (
    json |> Json.get("src") |?> Json.string,
    json |> Json.get("signature") |?> Json.string,
  ) {
  | (Some(src), Some(signature)) => Ok(Logoff({src, signature}))
  | _ => Error("Logoff message invalid format")
  };

let decodeOfferOrAnswer = json =>
  switch (
    json |> Json.get("src") |?> Json.string,
    json |> Json.get("tg") |?> Json.string,
    json |> Json.get("sdp") |?> Json.string,
    json |> Json.get("signature") |?> Json.string,
  ) {
  | (Some(src), Some(tg), Some(sdp), Some(signature)) =>
    Ok({src, tg, sdp, signature})
  | _ => Error("Offer message invalid format")
  };

let fromJSON = str => {
  let maybeJson =
    switch (Json.parse(str)) {
    | json => Some(json)
    | exception _ => None
    };

  switch (maybeJson) {
  | Some(json) =>
    let maybeVersion =
      switch (json |> Json.get("version")) {
      | None => Some(latestVersion)
      | Some(v) => v |> Json.number |?>> int_of_float
      };
    switch (maybeVersion) {
    | Some(_ver) =>
      /* TODO: Support different versions */
      switch (json |> Json.get("type") |?> Json.string) {
      | Some(typeStr) =>
        switch (typeStr) {
        | "login" => decodeLoginMsg(json)
        | "offer" =>
          switch (decodeOfferOrAnswer(json)) {
          | Ok(offerPayload) => Ok(Offer(offerPayload))
          | Error(_) as e => e
          }
        | "answer" =>
          switch (decodeOfferOrAnswer(json)) {
          | Ok(answerPayload) => Ok(Answer(answerPayload))
          | Error(_) as e => e
          }
        | "logoff" => decodeLogoffMsg(json)
        | _ => Error("Type: Unknown message type.")
        }
      | None => Error("Type: Missing or not string")
      }
    | None => Error("Version: Not a number.")
    };
  | None => Error("Not a valid JSON")
  };
};