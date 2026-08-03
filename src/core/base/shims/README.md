# Shims

One line each, and nothing else: they re-export a module from one of the
`liquidsoap-lang` libraries under its short name.

Those libraries are wrapped, so from here `Type` would have to be spelled
`Liquidsoap_lang_types.Type`, `Value` would be `Liquidsoap_lang_values.Value`,
and so on -- about 800 times across `src/core`. `liquidsoap_core` is itself
unwrapped, so a module here called `type.ml` makes `Type` available to the whole
of core, which is what these files are for.

To use another module from a `liquidsoap-lang` library, add its shim here and
add the library to `src/core/base/dune`. If the module is not exported by its
library's top-level `.mli` (`src/lang/<dir>/liquidsoap_lang_<dir>.mli`), it is
private on purpose -- export it there first, deliberately.

A file stops belonging here as soon as it does anything beyond the `include`.
`doc.ml`, `lang_string.ml`, `modules.ml` and `startup.ml` all start with one but
then add to it, so they live in `src/core/base` with the rest of core.
