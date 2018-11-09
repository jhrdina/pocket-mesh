const path = require("path");

module.exports = {
  entry: {
    SimpleCryptoDemo: "./example/SimpleCryptoDemo.bs.js",
    StoreDemo: "./example/StoreDemo.bs.js"
  },
  mode: "development",
  // optimization: {
  //   usedExports: true
  // },
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "../lib")
  }
};
