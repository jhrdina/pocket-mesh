open Webapi;
module type Demo = {let run: unit => unit;};

/* ========================================== */

let demos: list((string, module Demo)) = [
  ("simple-crypto-example-btn", (module SimpleCryptoDemo)),
  ("store-example-btn", (module StoreDemo)),
];

/* ========================================== */

let regDemo: (string, (module Demo)) => unit =
  (id, (module D)) =>
    switch (Dom.document |> Dom.Document.getElementById(id)) {
    | Some(btn) => btn |> Dom.Element.addClickEventListener(_ => D.run())
    | None => Js.log("Demo button with ID " ++ id ++ " not found")
    };

demos |> List.iter(((id, demoModule)) => regDemo(id, demoModule));