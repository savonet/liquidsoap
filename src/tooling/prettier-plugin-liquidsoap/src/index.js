const { languages, printers } = require("./base");
const liquidsoap = require("../dist/liquidsoap");

module.exports = {
  languages,
  printers,
  parsers: {
    liquidsoap: {
      parse: liquidsoap.lang.parse,
      astFormat: "liquidsoap",
      locStart: () => 0,
      locEnd: () => 0,
    },
  },
};
