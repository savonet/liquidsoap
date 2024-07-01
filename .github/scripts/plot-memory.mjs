import fs from "node:fs";
import path from "node:path";

const sha = process.argv[2];

const base = "artifacts";

const files = fs
  .readdirSync(base)
  .filter((dir) => /memory-usage$/.test(dir))
  .map((dir) =>
    fs
      .readdirSync(path.join(base, dir))
      .filter((json) => /memory-usage.json$/.test(json))
      .map((json) => path.join(base, dir, json)),
  )
  .flat();

const categories = [];
const shared_memory = [];
const ocaml_memory = [];
const non_ocaml_memory = [];

files.forEach((filename) => {
  const category = path.basename(filename).split("-")[0];

  const data = fs.readFileSync(filename);

  const {
    process_managed_memory,
    process_physical_memory,
    process_private_memory,
  } = JSON.parse(data);

  const process_shared_memory =
    process_physical_memory - process_private_memory;

  const process_non_ocaml_memory =
    process_private_memory - process_managed_memory;

  categories.push(category);
  non_ocaml_memory.push(process_non_ocaml_memory);
  ocaml_memory.push(process_managed_memory);
  shared_memory.push(process_shared_memory);
});

console.log(
  JSON.stringify({
    chart: {
      type: "column",
    },

    title: {
      text: `Liquidsoap memory consumption by feature (SHA: ${sha})`,
      align: "left",
    },

    xAxis: {
      categories,
      labels: {
        step: 1,
      },
    },

    yAxis: {
      allowDecimals: false,
      min: 0,
      title: {
        text: "Memory usage",
      },
    },

    plotOptions: {
      column: {
        stacking: "normal",
      },
    },

    series: [
      {
        name: "OCaml Memory",
        data: ocaml_memory,
      },
      {
        name: "Non OCaml Memory",
        data: non_ocaml_memory,
      },
      {
        name: "Shared Memory",
        data: shared_memory,
      },
    ],
  }),
);
