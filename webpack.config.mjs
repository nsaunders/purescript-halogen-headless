import path from "path";
import { fileURLToPath } from "url";
import HtmlPlugin from "html-webpack-plugin";
import MiniCssExtractPlugin from "mini-css-extract-plugin";
import VirtualModulesPlugin from "webpack-virtual-modules";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default ({ production }) => ({
  mode: production ? "production" : "development",
  entry: {
    app: path.resolve(__dirname, "app.virtual.mjs"),
  },
  output: {
    path: path.resolve(__dirname, "public"),
    filename: `[name]${production ? ".[contenthash]" : ""}.js`,
  },
  plugins: [
    new HtmlPlugin({ title: "Halogen Headless" }),
    new MiniCssExtractPlugin({
      filename: `[name]${production ? ".[contenthash]" : ""}.css`,
    }),
    new VirtualModulesPlugin({
      [path.resolve(__dirname, "app.virtual.css")]: `
        module.exports = () =>
          import("execa")
            .then(({ execa }) => execa("node", ["-e", "import(require('path').resolve(__dirname, 'output', 'Site.CSS', 'index.js')).then(({ sheet }) => process.stdout.write(sheet))"]))
            .then(({ stdout: code }) => ({
              code: \`
                @import "@fontsource/noto-sans";
                @import "@fontsource/noto-sans-mono";
                \${code}
              \`,
            }))
      `,
      [path.resolve(__dirname, "app.virtual.mjs")]: `
        import "./app.virtual.css";
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
        test: path.resolve(__dirname, "app.virtual.css"),
        use: "val-loader",
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
