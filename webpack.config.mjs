import path from "path";
import { fileURLToPath } from "url";
import HtmlPlugin from "html-webpack-plugin";
import MiniCssExtractPlugin from "mini-css-extract-plugin";
import VirtualModulesPlugin from "webpack-virtual-modules";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default ({ production }) => ({
  mode: production ? "production" : "development",
  entry: {
    app: [
      "./app.virtual.js",
      path.join(__dirname, "output", "Site.CSS", "index.js?css"),
    ],
  },
  output: {
    path: path.join(__dirname, "public"),
    filename: `[name]${production ? ".[contenthash]" : ""}.js`,
  },
  plugins: [
    new HtmlPlugin({ title: "Halogen Headless" }),
    new MiniCssExtractPlugin({
      filename: `[name]${production ? ".[contenthash]" : ""}.css`,
    }),
    new VirtualModulesPlugin({
      "./app.virtual.js": `
        import { main } from "./output/Site.Main/index.js";
        main();
      `,
    }),
  ],
  resolve: {
    extensions: [".js", ".css"],
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          "css-loader",
        ],
      },
      {
        test: /\.js$/,
        resourceQuery: /css/,
        use: [
          MiniCssExtractPlugin.loader,
          "css-loader",
          "execute-module-loader?export=sheet",
        ],
      },
      {
        test: /\.woff2?$/,
        type: "asset/resource",
      },
    ],
  },
  watchOptions: {
    aggregateTimeout: 500,
  },
  devServer: {
    hot: false,
  },
});
