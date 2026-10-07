# Language notes: dune and the toolchain

Advice about meeting [build.md](../build.md) and
[cross-compilation.md](../cross-compilation.md) with dune. It is not part of
the contract.

## Gating a library

- `enabled_if` can read a file produced by a rule and compare its whole
  content with a string. A trailing newline in the file makes the comparison
  false.
- `(optional)` on a library makes dune drop it silently when a dependency does
  not resolve. Combined with an availability gate it turns a broken stanza
  into a green build with the stubs uncompiled. Use `enabled_if` alone.
- A package whose only library is disabled is empty. Declaring packages as
  allowed to be empty makes that legal, which is why the install-time check
  exists: a rule with no target, attached to the install alias and scoped to
  the package, runs under the opam build command.

## Flags

- Compile and link flags can be included from a generated s-expression file.
  Each flag must be one atom: quote what needs quoting.
- Including flags without the standard set replaces dune's default C flags.
  The compiler driver still adds the OCaml configuration's own.
- A file with one flag per line can be expanded into separate arguments of a
  `run` action. That is the way to pass flags to a build-time program without
  a shell.
- Disable dune's own standard C and C++ flags at project level when the flags
  come from pkg-config.

## Detection

- A rule that depends on the whole environment re-runs on every build. When
  its output is unchanged, nothing downstream re-runs. For rule D5 the output
  must change with what it describes: include the versions pkg-config reports.
- dune does not track environment variables read by an action unless they are
  declared.
- dune-configurator offers pkg-config queries with a version expression. Its
  `main` entry point parses the command line itself; a program that takes its
  own arguments creates the configurator directly.
- A build-time program must get the target's OS type and context name from the
  build system's variables. Its own runtime describes the build machine.

## Generated code

- A generated OCaml module becomes a module of the library when no module list
  restricts the stanza.
- A foreign stub's compilation depends on every header in its directory, so a
  header generated there is built before the stub with no extra rule.
- A header installed by one library is found by the stubs of a library that
  depends on it: dune adds an include directory per dependency.
- The C compiler command expands to the compiler and its configured flags as
  one string. Pass it as one argument and split it in the program.
- Run the preprocessor with an argument vector, not through a shell.
- `-E` gives the enumerations of a header after conditionals are resolved.
  With GCC and Clang, `-dM -E` lists the macros defined at the end of
  preprocessing; its order is not the declaration order.
- Preprocessor output contains line markers and everything the header
  includes. Locate an enumeration by its `enum Name {` opening and its closing
  brace, not by line patterns.
- The variant constant of [build.md](../build.md) §3.6 is a pure function of
  the name. Computing it in the generator's own language keeps the generator
  free of C stubs and of the host runtime's word size.
- A build-time program must build where no native compiler exists: do not
  force native executables.

## Tests and examples

- Stanzas produced by a program are included with a dynamic include; a plain
  include cannot name a build product.
- A module can belong to one stanza only. Programs that share a helper module
  are declared in one stanza.
- An alias whose rules are all disabled does not exist; requesting it on the
  command line is an error, while an umbrella alias that depends on it stays
  green. A rule that is always present and fails with a message is what makes
  the absence loud everywhere.
- Files written by a test step are not build targets. A step that stops
  writing its file leaves the previous run's copy for later steps. Write into
  a directory created for the run.
- The result of an alias is not recomputed when a shared library loaded at run
  time changes. Make the suite depend on what detection reports.

## Cross-compilation

- `dune -x windows` creates a host context and a target context. A rule of the
  target context that runs a program runs the host context's build of it.
- The install alias skips the host context.
- A dependency needed by a build-time program and by a binding library is
  listed for both: the plain package and its `-windows` counterpart.

## Embedding

- A nested project file makes the directory its own dune project inside an
  enclosing workspace. Its packages are visible workspace-wide, and the
  workspace can set environment variables for its contexts.
