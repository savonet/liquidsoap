import { parsers as baseParsers } from "./base.mjs";
export { languages, printers } from "./base.mjs";

const liquidsoap = require("../dist/liquidsoap");

export const parsers = {
  liquidsoap: {
    ...baseParsers.liquidsoap,
    parse: liquidsoap.lang.parse,
  },
};
