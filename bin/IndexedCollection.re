module Make = (Primary: {type t; let compare: (t, t) => int;}) => {
  module PrimarySet = Set.Make(Primary);

  type index('elt, 'key) = {
    data: Hashtbl.t('key, PrimarySet.t),
    getKey: 'elt => 'key,
  };

  type iMem('elt) =
    | I(index('elt, 'key)): iMem('elt);

  type t('elt) = {
    getPrimary: 'elt => Primary.t,
    indices: list(iMem('elt)),
  };

  let add = (elt, t) =>
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
           oldPrimarySet |> PrimarySet.add(t.getPrimary(elt)),
         );
       });

  let remove = (elt, t) =>
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

  let makeIndex = getKey => {data: Hashtbl.create(10), getKey};

  let make = (getPrimary, indices) => {getPrimary, indices};
};