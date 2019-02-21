type signalState =
  | Offline
  | Online;

type peerState =
  | Offline
  | Loading
  | Online
  | OnlineNoDoc;

module Styles = {
  open Css;

  let spin =
    keyframes([
      (0, [transform(rotate(`deg(0)))]),
      (100, [transform(rotate(`deg(360)))]),
    ]);

  let spinner =
    style([
      animationName(spin),
      animationDuration(1000),
      animationIterationCount(`infinite),
      animationTimingFunction(`linear),
      transformOrigin(`px(18), `px(17)),
    ]);
};

let component = ReasonReact.statelessComponent("GlobalIcon");

let make = (~signalState: signalState, ~peerState: peerState, _children) => {
  ...component,
  render: _self =>
    <MaterialUi.Tooltip title={"Default"->ReasonReact.string}>
      <MaterialUi.SvgIcon>
        <path
          d="M11.2 10l-.113.015-.26.047.355 1.97.205-.038.58-.038.561.027.301.05.328-1.97-.352-.058-.115-.015-.688-.034h-.115l-.688.048m3.19 2.57l.266.147.459.324.43.387.143.16 1.5-1.33-.187-.213-.082-.08-.508-.455-.088-.07-.555-.393-.096-.061-.312-.172-.965 1.75m-6.15-1.53l-.093.06-.55.4-.087.073-.502.463-.078.082-.125.144 1.52 1.3.082-.094.414-.385.473-.346.332-.189-.99-1.74-.391.223"
        />
        <circle id="thisPeerOnline" cx="6" cy="17" r="3" />
        {switch (signalState) {
         | Offline =>
           <path
             id="signalOffline"
             d="M10.2 2.82L8.79 4.23 10.56 6 8.79 7.77l1.41 1.41 1.77-1.77 1.77 1.77 1.41-1.41L13.38 6l1.77-1.77-1.41-1.41-1.77 1.77-1.77-1.77"
           />
         | Online => <circle id="signalOnline" cx="12" cy="6" r="3" />
         }}
        {switch (peerState) {
         | Offline =>
           <path
             id="peerOffline"
             d="M16.2 13.8l-1.41 1.41 1.77 1.77-1.77 1.77 1.41 1.42 1.77-1.77 1.77 1.77 1.42-1.41-1.77-1.77 1.77-1.77-1.41-1.41-1.77 1.77-1.77-1.77z"
           />
         | Loading =>
           <path
             id="peerLoading"
             d="M17.7 14a3.016 3.016 0 0 0-2.59 2.31l1.95.445a.994.994 0 0 1 .861-.772.986.986 0 0 1 1.01.559.993.993 0 0 1-.191 1.14l1.42 1.41c.897-.9 1.13-2.28.576-3.42a3.024 3.024 0 0 0-3.04-1.68"
             className=Styles.spinner
           />
         | Online => <circle id="peerOnline" cx="18" cy="17" r="3" />
         | OnlineNoDoc =>
           <path
             id="peerOnlineNoDoc"
             d="M18 14c-1.66 0-3 1.34-3 3s1.34 3 3 3 3-1.34 3-3-1.34-3-3-3m0 1.5a1.5 1.5 0 1 1-.001 3 1.5 1.5 0 0 1 .001-3"
           />
         }}
      </MaterialUi.SvgIcon>
    </MaterialUi.Tooltip>,
  /*<path
      id="signalLoading"
      d="M9.41 4.48a3.008 3.008 0 0 0 .562 3.73 3.013 3.013 0 0 0 3.77.231 3 3 0 0 0 1.01-3.63l-1.83.793a.987.987 0 0 1-.338 1.21.997.997 0 0 1-1.26-.076.986.986 0 0 1-.187-1.24l-1.72-1.01"
    />*/
};