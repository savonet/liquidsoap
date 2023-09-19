const path = require("path");

module.exports = {
  entry: "./src/base.js",
  mode: "production",
  experiments: {
    outputModule: true,
  },
  output: {
    path: __dirname,
    library: {
      type: "module",
    },
  },
};
