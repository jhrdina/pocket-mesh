open WebapiExtra;

type t = {
  mutable db: option(Dom.IDBDatabase.t),
  dbName: string,
  objectStoreName: string,
};

let make = (dbName, objectStoreName) => {db: None, dbName, objectStoreName};

exception DatabaseAlreadyOpen;
let open_ = s =>
  Js.Promise.make((~resolve, ~reject) => {
    let req = Dom.indexedDB |> Dom.IDBFactory.open_(s.dbName, 1);
    req->Dom.IDBOpenDBRequest.setOnSuccess(_ => {
      s.db = req |> Dom.IDBOpenDBRequest.result;
      resolve(. Js.undefined);
    });
    req->Dom.IDBOpenDBRequest.setOnError(evt => {
      Js.log(evt);
      reject(. Not_found);
    });
    req->Dom.IDBOpenDBRequest.setOnBlocked(evt => {
      Js.log(evt);
      reject(. DatabaseAlreadyOpen);
    });
    req->Dom.IDBOpenDBRequest.setOnUpgradeNeeded(_ => {
      let db = req |> Dom.IDBOpenDBRequest.result;
      if (!(
            db
            |> Dom.IDBDatabase.objectStoreNames
            |> Dom.DOMStringList.contains(s.objectStoreName)
          )) {
        db
        |> Dom.IDBDatabase.createObjectStoreWithOptions(
             s.objectStoreName,
             Dom.IDBObjectStoreParameters.make(~autoIncrement=true, ()),
           )
        |> ignore;
      };
      s.db = Some(db);
    });
  });
exception KeyStoreNotOpen;
let saveKey = (id, thisPeer, s) =>
  Js.Promise.make((~resolve, ~reject) =>
    switch (s.db) {
    | Some(db) =>
      let transaction =
        db |> Dom.IDBDatabase.transaction([|s.objectStoreName|], ReadWrite);
      transaction->Dom.IDBTransaction.setOnError(evt => {
        Js.log(evt);
        reject(. Not_found);
      });
      transaction->Dom.IDBTransaction.setOnAbort(evt => {
        Js.log(evt);
        /* TODO: some real error please... */
        reject(. Not_found);
      });
      transaction->Dom.IDBTransaction.setOnComplete(_ =>
        resolve(. Js.undefined)
      );

      let objectStore =
        transaction |> Dom.IDBTransaction.objectStore(s.objectStoreName);
      objectStore |> Dom.IDBObjectStore.putWithKey(thisPeer, id) |> ignore;
    | None => reject(. KeyStoreNotOpen)
    }
  );

let getKey = (id, s) =>
  Js.Promise.make((~resolve, ~reject) =>
    switch (s.db) {
    | Some(db) =>
      let transaction =
        db |> Dom.IDBDatabase.transaction([|s.objectStoreName|], ReadOnly);
      let objectStore =
        transaction |> Dom.IDBTransaction.objectStore(s.objectStoreName);

      let request = objectStore |> Dom.IDBObjectStore.get(id);
      request->Dom.IDBRequest.setOnSuccess(_ =>
        resolve(. request |> Dom.IDBRequest.result)
      );
      request->Dom.IDBRequest.setOnError(_
        /* TODO: some real error please... */
        => reject(. Not_found));
    | None => reject(. KeyStoreNotOpen)
    }
  );

let close = s =>
  Js.Promise.make((~resolve, ~reject) =>
    switch (s.db) {
    | Some(db) =>
      db |> Dom.IDBDatabase.close;
      s.db = None;
      resolve(. Js.undefined);
    | None => reject(. KeyStoreNotOpen)
    }
  );