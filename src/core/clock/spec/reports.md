# Text reports — as built (Part A)

Three reports. All are plain text built from a tree of records; none reads a
clock directly, so each is a pure function of its input.

A **source entry** is an id and a list of activation ids, printed
`<id> [<activation>, <activation>]`, and `<id> []` with none.

Lines are wrapped at 100 characters unless stated. Wrapping only ever breaks
between two items, leaving a trailing comma on the broken line. The label width
used to indent continuation lines is counted in characters, not bytes; the
line length test is in bytes.

## 1. Source list

Used in the leak warning ([clock.md §7](clock.md#7-attaching-sources)).

Input: a clock name, its outputs, active and passive source entries, and its
started sub-clocks, recursively.

```
parent:
├── outputs = [out1 []]
├── passive = [p1 []]
└─ child:
   │ outputs = [out2 []]
   │ active  = [a1 []]
```

- First line: the name and `:`.
- One field per non-empty list, in the order `outputs`, `active `, `passive`
  (labels padded to 7 characters). Empty lists are omitted.
- Items are sorted by id.
- At the root, a field is prefixed `├── `, or `└── ` when it is the last field
  and there are no sub-clocks. Below the root, every field is prefixed `│ `.
- A line holds items while its length plus one stays within the width.
  Continuation lines are indented to the column after `[`.
- Sub-clocks follow, each prefixed `├─ `, the last `└─ `. Their own lines are
  indented by `│  `, or three spaces under the last one.

## 2. Clock dump

Input: for each clock its name, ticks, stream time, self-sync flag, the three
source lists, and its sub-clocks, recursively.

```
· parent (ticks: 100, time: 2.50s, self_sync: false)
  ├── outputs: output []
  ├── active sources:
  ├── passive sources: src1 []
  └── child (ticks: 50, time: 1.25s, self_sync: true)
      ├── outputs: child_out []
      ├── active sources: child_active []
      └── passive sources:
```

- Header: `<name> (ticks: <n>, time: <seconds, 2 decimals>s, self_sync: <bool>)`,
  prefixed `· ` at the root, and `├── ` or `└── ` for a sub-clock.
- Three fields always, in the order `outputs`, `active sources`,
  `passive sources`, each `<label>: <items>`. An empty list prints the label
  and colon alone. Items keep their input order.
- `passive sources` is prefixed `└── ` when there are no sub-clocks.
- A line holds items while its length stays within the width.
- Root clocks are separated by one empty line.

The application's dump covers "the clocks of the application"
([clock.md §5](clock.md#5-registry)). A stopped clock appears with 0 ticks,
time −1.00 and empty lists.

## 3. Source graph

Input: a flat list of sources, each with a name, a type and the names of what
activates it. If `S` lists `A` as an activation, `S` is printed under `A`.

```
Outputs:
· output.icecast [output]
  └── shared_encoder [passive]
      └── audio [passive]
· output.file [output]
  └── shared_encoder [passive] (*)
```

- A source's own name among its activations is ignored.
- An activation name that matches no source in the list is an **external
  activator**.
- Section `Outputs:` lists roots prefixed `· `:
  1. each external activator of an output, sorted by name, printed
     `<name> [external activation]`, with the outputs it activates beneath;
  2. then each output with no external activator, `<name> [<type>]`.

  ```
  Outputs:
  · external_clock [external activation]
    └── output [output]
        └── encoder [passive]
  ```

- Under a source come the sources it activates, in input order, prefixed
  `├── ` and `└── ` for the last, indented four columns per level.
- A source already printed is printed again as `<name> [<type>] (*)` with
  nothing beneath it.
- Section `Singletons:` lists every source not printed above, the same way:
  external activators first, then the rest as `· <name> [<type>]`.
- The two sections are separated by one empty line. An empty section is
  omitted. No wrapping.

The per-clock report applies this to all the clock's sources, pending ones
included. The whole-application report prints, for each clock,
`Clock <id>:` followed by its graph, then the same for each sub-clock with the
header `Clock <id> (controlled by <parent id>):`, all separated by one empty
line.
