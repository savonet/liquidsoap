import child_process from "child_process";
import fs from "fs";

export { languages, printers } from "./base.mjs";

export const parse = (input) => {
  const [process, args] = fs.existsSync("../json_dump.exe")
    ? ["../json_dump.exe", []]
    : ["dune", ["exec", "../json_dump.exe"]];
  return JSON.parse(
    child_process.execFileSync(process, args, {
      input,
      maxBuffer: Number.MAX_SAFE_INTEGER,
    }),
  );
};

export const parsers = {
  liquidsoap: {
    parse,
    astFormat: "liquidsoap",
    locStart: () => 0,
    locEnd: () => 0,
  },
};
