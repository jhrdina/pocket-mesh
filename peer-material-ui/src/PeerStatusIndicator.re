type signalState =
  | Offline
  | Online;

type inGroup = bool;

type connectionState =
  | Offline
  | Connecting
  | Connected
  | ConnectedTransferring;

module Styles = {
  open Css;

  let activeColor = `hex("7cb342");

  let wrapper =
    style([
      position(`relative),
      overflow(`hidden),
      width(`px(40)),
      height(`px(8)),
    ]);

  let line =
    style([
      backgroundColor(`hex("ccc")),
      height(`px(2)),
      width(`px(30)),
      margin2(~v=`px(3), ~h=`zero),
      transform(translateX(`px(-30))),
      transitions([
        transition(
          "transform",
          ~duration=300,
          ~timingFunction=`cubicBezier((0.0, 0.8, 1.0, 1.0)),
        ),
        transition(
          "background-color",
          ~duration=300,
          ~timingFunction=`cubicBezier((0.0, 0.8, 1.0, 1.0)),
        ),
      ]),
    ]);

  let lineInterested = style([transform(`translateX(`px(-22)))]);

  let lineConnecting =
    style([
      transform(translateX(`px(-2))),
      transitions([
        transition(
          "transform",
          ~duration=20000,
          ~timingFunction=`cubicBezier((0.2, 0.8, 1.0, 1.0)),
        ),
        transition(
          "background-color",
          ~duration=1000,
          ~timingFunction=`cubicBezier((0.0, 0.8, 1.0, 1.0)),
        ),
      ]),
    ]);

  let lineConnected =
    style([backgroundColor(activeColor), transform(`translateX(`zero))]);

  let hamsterAnimation =
    keyframes([
      (0, [transforms([`scaleY(0.4), `rotate(`deg(45))])]),
      (
        40,
        [
          transforms([
            `translateX(`px(17)),
            `scaleY(0.3),
            `rotate(`deg(45)),
          ]),
        ],
      ),
      (
        83,
        [
          transforms([
            `translateX(`px(27)),
            `scaleY(0.19),
            `rotate(`deg(45)),
          ]),
        ],
      ),
      (
        100,
        [
          animationTimingFunction(`cubicBezier((0.0, 0.0, 1.0, 0.5))),
          transforms([
            `translateX(`px(30)),
            `scaleY(0.12),
            `rotate(`deg(45)),
          ]),
        ],
      ),
    ]);

  let hamster =
    style([
      backgroundColor(activeColor),
      position(absolute),
      width(`px(16)),
      height(`px(16)),
      borderTopLeftRadius(`px(7)),
      borderTopRightRadius(`px(5)),
      borderBottomLeftRadius(`px(5)),
      borderBottomRightRadius(`px(7)),
      animation(
        hamsterAnimation,
        ~duration=1000,
        ~timingFunction=`linear,
        ~iterationCount=`infinite,
      ),
      top(`px(-4)),
      left(`px(-17)),
      display(`none),
    ]);

  let hamsterActive = style([display(`block)]);

  let point =
    style([
      backgroundColor(`hex("7cb342")),
      position(`absolute),
      height(`px(8)),
      width(`px(8)),
      top(`zero),
      right(`zero),
      borderRadius(`percent(50.0)),
      transform(`scale((0.0, 0.0))),
      transition(
        "transform",
        ~duration=3000,
        ~timingFunction=`cubicBezier((0.2, 0.8, 1.0, 1.0)),
      ),
    ]);

  let pointOnline = style([transform(`scale((1.0, 1.0)))]);
};

let component = ReasonReact.statelessComponent("PeerStatusIndicator");

let classes = String.concat(" ");

let make =
    (
      ~signalState: signalState,
      ~inGroup: bool,
      ~connectionState: connectionState,
      ~className="",
      _children,
    ) => {
  ...component,

  render: _self =>
    <div className={classes([Styles.wrapper, className])}>
      <div
        className={classes(
          Styles.[
            line,
            inGroup ? lineInterested : "",
            switch (connectionState) {
            | Offline => ""
            | Connecting => lineConnecting
            | Connected
            | ConnectedTransferring => lineConnected
            },
          ],
        )}
      />
      <div
        className={classes(
          Styles.[
            hamster,
            switch (connectionState) {
            | ConnectedTransferring => hamsterActive
            | _ => ""
            },
          ],
        )}
      />
      <div
        className={classes(
          Styles.[
            point,
            switch (signalState) {
            | Online => pointOnline
            | Offline => ""
            },
          ],
        )}
      />
    </div>,
};