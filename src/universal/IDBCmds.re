open WebapiExtra;
open BlackTea;

type t = {
  db: Dom.IDBDatabase.t,
  objectStoreName: string,
};

exception DatabaseAlreadyOpen;
exception RequestError;
let open_ = (dbName, objectStoreName, dbToMsg, errorToMsg) =>
  Cmd.call(callbacks => {
    let req = Dom.indexedDB |> Dom.IDBFactory.open_(dbName, 1);
    req->Dom.IDBOpenDBRequest.setOnSuccess(_ =>
      callbacks^.enqueue(
        dbToMsg @@ {db: req |> Dom.IDBOpenDBRequest.result, objectStoreName},
      )
    );
    req->Dom.IDBOpenDBRequest.setOnError(evt => {
      Js.log(evt);
      callbacks^.enqueue(errorToMsg @@ RequestError);
    });
    req->Dom.IDBOpenDBRequest.setOnBlocked(evt => {
      Js.log(evt);
      callbacks^.enqueue(errorToMsg @@ DatabaseAlreadyOpen);
    });
    req->Dom.IDBOpenDBRequest.setOnUpgradeNeeded(_ => {
      let db = req |> Dom.IDBOpenDBRequest.result;
      if (!(
            db
            |> Dom.IDBDatabase.objectStoreNames
            |> Dom.DOMStringList.contains(objectStoreName)
          )) {
        db
        |> Dom.IDBDatabase.createObjectStoreWithOptions(
             objectStoreName,
             Dom.IDBObjectStoreParameters.make(~autoIncrement=true, ()),
           )
        |> ignore;
      };
    });
  });

exception KeyStoreNotOpen;
exception TransactionError;
exception TransactionAbort;
let setKey = (id, value, successToMsg, errorToMsg, t) =>
  Cmd.call(callbacks => {
    let transaction =
      t.db |> Dom.IDBDatabase.transaction([|t.objectStoreName|], ReadWrite);
    transaction->Dom.IDBTransaction.setOnError(evt => {
      Js.log(evt);
      callbacks^.enqueue(errorToMsg @@ TransactionError);
    });
    transaction->Dom.IDBTransaction.setOnAbort(evt => {
      Js.log(evt);
      callbacks^.enqueue(errorToMsg @@ TransactionAbort);
    });
    transaction->Dom.IDBTransaction.setOnComplete(_ =>
      callbacks^.enqueue(successToMsg())
    );

    transaction
    |> Dom.IDBTransaction.objectStore(t.objectStoreName)
    |> Dom.IDBObjectStore.putWithKey(value, Dom.idbKeyOfString(id))
    |> ignore;
  });

let getKey = (id, valueToMsg, errorToMsg, t) =>
  Cmd.call(callbacks => {
    let transaction =
      t.db |> Dom.IDBDatabase.transaction([|t.objectStoreName|], ReadOnly);
    let objectStore =
      transaction |> Dom.IDBTransaction.objectStore(t.objectStoreName);

    let request =
      objectStore |> Dom.IDBObjectStore.get(Dom.idbKeyOfString(id));
    request->Dom.IDBRequest.setOnSuccess(_ =>
      callbacks^.enqueue(valueToMsg(request |> Dom.IDBRequest.result))
    );
    request->Dom.IDBRequest.setOnError(_
      /* TODO: some real error please... */
      => callbacks^.enqueue(errorToMsg @@ RequestError));
  });

let close = (successToMsg, t) =>
  Cmd.call(callbacks => {
    t.db |> Dom.IDBDatabase.close;
    callbacks^.enqueue(successToMsg());
  });