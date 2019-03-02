open BlackTea;

let create = () => {
  let prevModelRef = ref(None);
  model => {
    let changed =
      switch (prevModelRef^) {
      | None => true
      | Some(prevModel) when prevModel !== model => true
      | Some(_prevModel) => false
      };
    if (changed) {
      Js.log(model);
    };
    Sub.none;
  };
};
/* let externalNotifier = model => model.Sub.none; */