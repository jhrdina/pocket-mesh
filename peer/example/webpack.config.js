const path = require("path");
const BundleAnalyzerPlugin = require("webpack-bundle-analyzer")
  .BundleAnalyzerPlugin;

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: {
    SimpleCryptoDemo: "./example/SimpleCryptoDemo.bs.js",
    StoreDemo: "./example/StoreDemo.bs.js"
  },
  mode: isProd ? "production" : "development",
  // optimization: {
  //   usedExports: true
  // },
  output: {
    filename: "[name].bundle.js",
    path: path.join(__dirname, "../lib")
  }
  // plugins: [new BundleAnalyzerPlugin()]
};
