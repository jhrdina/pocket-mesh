open MaterialUi.SvgIcon;

module type IconRef = {let reactClass: ReasonReact.reactClass;};

module BuildIcon = (Base: IconRef) => {
  let reactClass = Base.reactClass;
  let make =
      (
        ~className: option(string)=?,
        ~color: option(color)=?,
        ~component:
           option(
             [
               | `String(string)
               | `Callback('genericCallback)
               | `ObjectGeneric(Js.t({..}))
             ],
           )=?,
        ~fontSize: option(fontSize)=?,
        ~nativeColor: option(string)=?,
        ~titleAccess: option(string)=?,
        ~viewBox: option(string)=?,
        ~classes: option(Classes.t)=?,
        ~style: option(ReactDOMRe.Style.t)=?,
        children,
      ) =>
    ReasonReact.wrapJsForReason(
      ~reactClass=Base.reactClass,
      ~props=
        makeProps(
          ~className?,
          ~color=?color->(Belt.Option.map(v => colorToJs(v))),
          ~component=?
            component->(
                         Belt.Option.map(v =>
                           MaterialUi_Helpers.unwrapValue(v)
                         )
                       ),
          ~fontSize=?fontSize->(Belt.Option.map(v => fontSizeToJs(v))),
          ~nativeColor?,
          ~titleAccess?,
          ~viewBox?,
          ~classes=?Belt.Option.map(classes, v => Classes.to_obj(v)),
          ~style?,
          (),
        ),
      children,
    );
};

module Add =
  BuildIcon({
    [@bs.module "@material-ui/icons/Add"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module ArrowBack =
  BuildIcon({
    [@bs.module "@material-ui/icons/ArrowBack"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module Clear =
  BuildIcon({
    [@bs.module "@material-ui/icons/Clear"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module Delete =
  BuildIcon({
    [@bs.module "@material-ui/icons/Delete"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module Info =
  BuildIcon({
    [@bs.module "@material-ui/icons/Info"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module MoreVert =
  BuildIcon({
    [@bs.module "@material-ui/icons/MoreVert"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module Person =
  BuildIcon({
    [@bs.module "@material-ui/icons/Person"]
    external reactClass: ReasonReact.reactClass = "default";
  });

module PersonAdd =
  BuildIcon({
    [@bs.module "@material-ui/icons/PersonAdd"]
    external reactClass: ReasonReact.reactClass = "default";
  });