(lang dune 3.23)
(version 0.5.3)
(name mad)
(source (github savonet/ocaml-mad))
(license GPL-2.0-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(subst disabled)
(use_standard_c_and_cxx_flags false)

(package
 (name mad)
 (allow_empty)
 (synopsis "Mad decoding library")
 (description "Bindings for the mad library which provides functions for encoding wave audio files into mp3")
 (depends
  (conf-pkg-config :build)
  (conf-mad :build)
  (ocaml (>= 4.08))
  dune-configurator)
)
