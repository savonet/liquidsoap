(lang dune 3.23)
(version 0.1.5)
(name dssi)
(source (github savonet/ocaml-dssi))
(license LGPL-2.1)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(subst disabled)
(use_standard_c_and_cxx_flags false)

(package
 (name dssi)
 (synopsis "Bindings for the DSSI API which provides audio synthesizers")
 (allow_empty)
 (depends
  conf-pkg-config
  dune-configurator
  (ocaml (>= 4.08))
  (ladspa (>= 0.2.1))
  conf-dssi)
)
