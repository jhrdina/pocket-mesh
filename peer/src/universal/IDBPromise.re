open WebapiExtra;
open BlackTea;

type t = {
  db: Dom.IDBDatabase.t,
  objectStoreName: string,
};

exception DatabaseAlreadyOpen;
exception RequestError;
let open_ = (dbName, objectStoreName) =>
  Js.Promise.make((~resolve, ~reject) => {
    let req = Dom.indexedDB |> Dom.IDBFactory.open_(dbName, 1);
    req->Dom.IDBOpenDBRequest.setOnSuccess(_ =>
      resolve(. {db: req |> Dom.IDBOpenDBRequest.result, objectStoreName})
    );
    req->Dom.IDBOpenDBRequest.setOnError(_evt => reject(. RequestError));
    req->Dom.IDBOpenDBRequest.setOnBlocked(evt => {
      Js.log(evt);
      reject(. DatabaseAlreadyOpen);
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

let deleteDatabase = dbName =>
  Js.Promise.make((~resolve, ~reject) => {
    let req = Dom.indexedDB |> Dom.IDBFactory.deleteDatabase(dbName);
    req->Dom.IDBOpenDBRequest.setOnSuccess(_ => {
      let s = ();
      resolve(. s);
    });
    req->Dom.IDBOpenDBRequest.setOnError(_evt => reject(. RequestError));
    req->Dom.IDBOpenDBRequest.setOnBlocked(evt => {
      Js.log(evt);
      reject(. DatabaseAlreadyOpen);
    });
  });

exception KeyStoreNotOpen;
exception TransactionError;
exception TransactionAbort;
let setKey = (id, value, t) =>
  Js.Promise.make((~resolve, ~reject) => {
    let transaction =
      t.db |> Dom.IDBDatabase.transaction([|t.objectStoreName|], ReadWrite);
    transaction->Dom.IDBTransaction.setOnError(evt => {
      Js.log(evt);
      reject(. TransactionError);
    });
    transaction->Dom.IDBTransaction.setOnAbort(evt => {
      Js.log(evt);
      reject(. TransactionAbort);
    });
    transaction->Dom.IDBTransaction.setOnComplete(_ => {
      let s = ();
      resolve(. s);
    });

    transaction
    |> Dom.IDBTransaction.objectStore(t.objectStoreName)
    |> Dom.IDBObjectStore.putWithKey(value, Dom.idbKeyOfString(id))
    |> ignore;
  });

let getKey = (id, t) =>
  Js.Promise.make((~resolve, ~reject) => {
    let transaction =
      t.db |> Dom.IDBDatabase.transaction([|t.objectStoreName|], ReadOnly);
    let objectStore =
      transaction |> Dom.IDBTransaction.objectStore(t.objectStoreName);

    let request =
      objectStore |> Dom.IDBObjectStore.get(Dom.idbKeyOfString(id));
    request->Dom.IDBRequest.setOnSuccess(_ =>
      resolve(. request |> Dom.IDBRequest.result)
    );
    request->Dom.IDBRequest.setOnError(_
      /* TODO: some real error please... */
      => reject(. RequestError));
  });

let close = t => t.db |> Dom.IDBDatabase.close;