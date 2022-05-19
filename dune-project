(lang dune 2.8)
(using menhir 2.1)
(using dune_site 0.1)

(package
  (name liquidsoap)
  (sites (share libs) (share bin) (lib_root lib_root)))

(package
 (name metadata)
 (synopsis "Read metadata from various file formats")
 (description "A pure OCaml library for reading files from various formats.")
 (depends
  camomile))
