module Make = (Primary: {type t; let compare: (t, t) => int;}) => {
  module PrimarySet = Set.Make(Primary);

  module Index = {
    type t('elt, 'key) = {
      data: Hashtbl.t('key, PrimarySet.t),
      getKey: 'elt => 'key,
    };
    let get = (key, index) =>
      switch (Hashtbl.find_opt(index.data, key)) {
      | Some(primarySet) => primarySet
      | None => PrimarySet.empty
      };

    let mem = (key, index) =>
      switch (Hashtbl.find_opt(index.data, key)) {
      | Some(primarySet) => ! (primarySet |> PrimarySet.is_empty)
      | None => false
      };

    let create = getKey => {data: Hashtbl.create(10), getKey};
  };

  type iMem('elt) =
    | I(Index.t('elt, 'key)): iMem('elt);

  type t('elt) = {
    records: Hashtbl.t(Primary.t, 'elt),
    getPrimary: 'elt => Primary.t,
    indices: list(iMem('elt)),
  };

  let add = (elt, t) => {
    let primaryKeyVal = t.getPrimary(elt);
    Hashtbl.replace(t.records, primaryKeyVal, elt);
    t.indices
    |> List.iter((I(index)) => {
         let key = index.getKey(elt);
         let oldPrimarySet =
           switch (Hashtbl.find_opt(index.data, key)) {
           | Some(primarySet) => primarySet
           | None => PrimarySet.empty
           };
         Hashtbl.replace(
           index.data,
           key,
           oldPrimarySet |> PrimarySet.add(primaryKeyVal),
         );
       });
  };

  let removeElt = (elt, t) => {
    let primaryKeyVal = t.getPrimary(elt);
    Hashtbl.remove(t.records, primaryKeyVal);
    t.indices
    |> List.iter((I(index)) => {
         let key = index.getKey(elt);
         switch (Hashtbl.find_opt(index.data, key)) {
         | Some(primarySet) =>
           let newPrimarySet =
             primarySet |> PrimarySet.remove(t.getPrimary(elt));
           if (newPrimarySet |> PrimarySet.is_empty) {
             Hashtbl.remove(index.data, key);
           } else {
             Hashtbl.replace(index.data, key, newPrimarySet);
           };
         | None => ()
         };
       });
  };

  let remove = (primaryKeyVal, t) =>
    switch (Hashtbl.find_opt(t.records, primaryKeyVal)) {
    | Some(elt) => removeElt(elt, t)
    | None => ()
    };

  let findOpt = (primaryKeyVal, t) =>
    Hashtbl.find_opt(t.records, primaryKeyVal);

  let mem = (primaryKeyVal, t) => Hashtbl.mem(t.records, primaryKeyVal);

  let create = (getPrimary, indices) => {
    records: Hashtbl.create(10),
    getPrimary,
    indices,
  };
};