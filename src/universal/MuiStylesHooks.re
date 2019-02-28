type style = {
  name: string,
  styles: ReactDOMRe.Style.t,
};

let generateDict = (lst: list(style)) => {
  let classDict: Js.Dict.t(ReactDOMRe.Style.t) = Js.Dict.empty();
  lst->Belt.List.forEach(style =>
    Js.Dict.set(classDict, style.name, style.styles)
  );
  classDict;
};

[@bs.module "@material-ui/styles"]
external makeWithTheme:
  (. (MaterialUi.Theme.t => Js.Dict.t(ReactDOMRe.Style.t))) =>
  (. unit) => Js.t({..}) =
  "makeStyles";
let makeWithTheme = stylesCreator =>
  makeWithTheme(. theme => stylesCreator(theme)->generateDict);

[@bs.module "@material-ui/styles"]
external make: (. Js.Dict.t(ReactDOMRe.Style.t)) => (. unit) => Js.t({..}) =
  "makeStyles";
let make = styles => make(. styles->generateDict);