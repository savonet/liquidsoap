export { languages, printers } from "./base.mjs";
import liquidsoap from "../dist/liquidsoap.js";

export const parsers = {
  liquidsoap: {
    parse: liquidsoap.lang.parse,
    astFormat: "liquidsoap",
    locStart: () => 0,
    locEnd: () => 0,
  },
};
