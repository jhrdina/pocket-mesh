const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const outputDir = path.join(__dirname, "lib");

const isProd = process.env.NODE_ENV === "production";

module.exports = {
  entry: {
    Overview: "./examples/Overview.js",
    TypicalUsage: "./examples/TypicalUsage.js"
  },
  mode: isProd ? "production" : "development",
  output: {
    path: outputDir,
    filename: "[name].bundle.js"
  },
  devServer: {
    compress: true,
    contentBase: path.join(__dirname, "examples"),
    port: process.env.PORT || 8000,
    historyApiFallback: true
  }
};
