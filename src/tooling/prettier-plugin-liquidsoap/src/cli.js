#!/usr/bin/env node

const parseArgs = require("minimist");
const prettier = require("prettier");
const fs = require("node:fs");
const prettierPluginLiquidsoap = require("../dist/node.js");

const {_: [filename], write, w} = parseArgs(process.argv.slice(2));

if (!filename) {
  console.error("No filename passed!");
  process.exit(1);
}

if (!fs.existsSync(filename)) {
  console.error(`File ${filename} does not exist!`);
  process.exit(1);
}

const code = "" + fs.readFileSync(filename);

prettier.format(
  code, {
    parser: "liquidsoap",
    plugins: [prettierPluginLiquidsoap]
  }).then((formattedCode) => {
  if (write || w) {
    console.log(`Writting formatted ${filename}`);
    fs.writeFileSync(filename, formattedCode);
  } else {
    process.stdout.write(formattedCode);
  }
  process.exit(0);
}).catch((err) => {
  console.error(`Error while processing file ${filename}: ${err}`);
  process.exit(1);
});
