/**
 * Copyright (c) 2017-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// See https://docusaurus.io/docs/site-config for all the possible
// site configuration options.

const reasonHighlightJs = require('reason-highlightjs');

// List of projects/orgs using your project for the users page.
const users = [
  {
    caption: 'User1',
    // You will need to prepend the image path with your baseUrl
    // if it is not '/', like: '/test-site/img/image.jpg'.
    image: '/img/undraw_open_source.svg',
    infoLink: 'https://www.facebook.com',
    pinned: true,
  },
];

const siteConfig = {
  title: 'PocketMesh', // Title for your website.
  tagline: 'Framework for creating collaborative P2P web apps',
  url: 'https://pocket-mesh.hrdinajan.cz', // Your website URL
  baseUrl: '/', // Base URL for your project */
  // For github.io type URLs, you would set the url and baseUrl like:
  //   url: 'https://facebook.github.io',
  //   baseUrl: '/test-site/',

  cname: 'pocket-mesh.hrdinajan.cz',

  // Used for publishing and more
  projectName: 'pocket-mesh',
  organizationName: 'jhrdina',
  // For top-level user or org sites, the organization is still the same.
  // e.g., for the https://JoelMarcey.github.io site, it would be set like...
  //   organizationName: 'JoelMarcey'

  // For no header links in the top nav bar -> headerLinks: [],
  headerLinks: [
    {doc: 'getting-started', label: 'Docs'},
    {page: 'api', label: 'API', external: true},
    {href: 'https://github.com/jhrdina/pocket-mesh', external: true,  label: 'GitHub'},
    {search: true}
  ],

  // If you have users set above, you add it here:
  users,

  /* path to images for header/footer */
  headerIcon: 'img/pocketmesh_white_transparent.svg',
  footerIcon: 'img/pocketmesh_white_transparent.svg',
  favicon: 'img/favicon.ico',

  /* Colors for website */
  colors: {
    primaryColor: '#616161',
    secondaryColor: '#545454',
  },

  /* Custom fonts for website */
  /*
  fonts: {
    myFont: [
      "Times New Roman",
      "Serif"
    ],
    myOtherFont: [
      "-apple-system",
      "system-ui"
    ]
  },
  */

  // This copyright info is used in /core/Footer.js and blog RSS/Atom feeds.
  copyright: `Copyright © ${new Date().getFullYear()} Jan Hrdina`,

  // highlight: {
  //   // Highlight.js theme to use for syntax highlighting in code blocks.
  //   theme: 'default',
  //   defaultLang: 'reasonml'
  // },
  highlight: {
    theme: 'atom-one-light',
    hljs: function(hljs) {
      hljs.registerLanguage('reason', reasonHighlightJs)
    }
  },

  separateCss: ['static/api'],

  // Add custom scripts here that would be placed in <script> tags.
  scripts: ['https://buttons.github.io/buttons.js'],

  // On page navigation for the current documentation page.
  onPageNav: 'separate',
  // No .html extensions for paths.
  cleanUrl: true,

  // Open Graph and Twitter card images.
  //ogImage: 'img/undraw_online.svg',
  //twitterImage: 'img/undraw_tweetstorm.svg',

  // Show documentation's last contributor's name.
  // enableUpdateBy: true,

  // Show documentation's last update time.
  enableUpdateTime: true,

  wrapPagesHTML: true

  // You may provide arbitrary config keys to be used as needed by your
  // template. For example, if you need your repo's URL...
  //   repoUrl: 'https://github.com/facebook/test-site',
};

module.exports = siteConfig;