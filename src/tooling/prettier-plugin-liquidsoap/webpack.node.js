const path = require("path");

module.exports = {
  entry: "./src/index.mjs",
  mode: "production",
  target: "node",
  output: {
    path: __dirname + "/dist",
    filename: "node.js",
    library: {
      type: "commonjs2",
    },
  },
};
