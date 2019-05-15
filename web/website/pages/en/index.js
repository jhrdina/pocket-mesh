/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const React = require("react");

const CompLibrary = require("../../core/CompLibrary.js");

const MarkdownBlock = CompLibrary.MarkdownBlock; /* Used to read markdown */
const Container = CompLibrary.Container;
const GridBlock = CompLibrary.GridBlock;

class HomeSplash extends React.Component {
  render() {
    const { siteConfig, language = "" } = this.props;
    const { baseUrl, docsUrl } = siteConfig;
    const docsPart = `${docsUrl ? `${docsUrl}/` : ""}`;
    const langPart = `${language ? `${language}/` : ""}`;
    const docUrl = doc => `${baseUrl}${docsPart}${langPart}${doc}`;

    const SplashContainer = props => (
      <div className="homeContainer">
        <div className="homeSplashFade">
          <div className="wrapper homeWrapper">{props.children}</div>
        </div>
      </div>
    );

    const Logo = props => (
      <div className="projectLogo">
        <img src={props.img_src} alt="Project Logo" />
      </div>
    );

    const ProjectTitle = () => (
      <h2 className="projectTitle">
        {siteConfig.title}
        <small>{siteConfig.tagline}</small>
      </h2>
    );

    const PromoSection = props => (
      <div className="section promoSection">
        <div className="promoRow">
          <div className="pluginRowBlock">{props.children}</div>
        </div>
      </div>
    );

    const Button = props => {
      let primaryCls = props.primary ? "button--primary" : "";
      let cls = `button ${primaryCls}`;
      return (
        <div className="pluginWrapper buttonWrapper">
          <a className={cls} href={props.href} target={props.target}>
            {props.children}
          </a>
        </div>
      );
    };

    return (
      <SplashContainer>
        { /* <Logo img_src={`${baseUrl}img/undraw_monitor.svg`} /> */ }
        <div className="inner">
          <ProjectTitle siteConfig={siteConfig} />
          <PromoSection>
            <Button primary href="https://tree-burst.hrdinajan.cz" target="_blank">
              Demo App
            </Button>
            <Button href={docUrl('getting-started.html')}>Get started</Button>
          </PromoSection>
        </div>
      </SplashContainer>
    );
  }
}

class Index extends React.Component {
  render() {
    const { config: siteConfig, language = "" } = this.props;
    const { baseUrl } = siteConfig;

    const Block = props => {
      let align = props.align || "center";
      return (
        <Container
          padding={["bottom", "top"]}
          id={props.id}
          background={props.background}
        >
          <GridBlock
            align={align}
            contents={props.children}
            layout={props.layout}
          />
        </Container>
      );
    };

    return (
      <div>
        <HomeSplash siteConfig={siteConfig} language={language} />
        <div className="mainContainer">
          <Block layout="threeColumn">
            {[
              {
                content:
                  "Open your webapp and edit your content offline, everything synchronizes as soon as you connect to the internet.",
                image: `${baseUrl}img/offline.svg`,
                imageAlign: "top",
                title: "Ready for offline"
              },
              {
                content:
                  "Create your own private mesh and let your data sync between your devices via WebRTC. No need to trust to the Big Brother.",
                image: `${baseUrl}img/no_middleman.svg`,
                imageAlign: "top",
                title: "No need for a central storage"
              },
              {
                content:
                  "Expressive types, pattern-matching, super-fast compile times and features of ECMAScript 2020 all at your fingertips.",
                image: `${baseUrl}img/reason-small.svg`,
                imageAlign: "top",
                title: "Written in Reason"
              },
            ]}
          </Block>

          <Block background="light" align="left">
            {[
              {
                content:
                  "Friends-list, groups management, permissions settings... don't create the same GUI for every app again and again. Use our pre-built React components and rather spend time working on your shiny new editor!\n\n",
                image: `${baseUrl}img/gui_screens.png`,
                imageAlign: "right",
                title: "Pre-built GUI components"
              }
            ]}
          </Block>

          <Block background="light" align="left">
            {[
              {
                title: "Check-out our demo P2P editor",
                content:
                  "We've created a simple P2P mind-map editor with offline support, conflict resolution and much more. [**Try it out**](https://tree-burst.hrdinajan.cz) or browse its [**source-code**](https://github.com/jhrdina/tree-burst) to learn more.",
                image: `${baseUrl}img/tree-burst.png`,
                imageAlign: "left"
              }
            ]}
          </Block>
        </div>
      </div>
    );
  }
}

module.exports = Index;
