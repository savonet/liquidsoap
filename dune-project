(lang dune 2.8)
(using menhir 2.1)
(using dune_site 0.1)
(name liquidsoap)
(source (github savonet/liquidsoap))
(license GPL-2.0-or-later)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")
(homepage "https://github.com/savonet/liquidsoap")
(bug_reports "https://github.com/savonet/liquidsoap/issues")

(version 2.1.0)
(generate_opam_files true)

(package
  (name liquidsoap)
  (synopsis "Swiss-army knife for multimedia streaming")
  (description
"\| Liquidsoap is a powerful and flexible language for describing your
"\| streams. It offers a rich collection of operators that you can combine
"\| at will, giving you more power than you need for creating or
"\| transforming streams. But liquidsoap is still very light and easy to
"\| use, in the Unix tradition of simple strong components working
"\| together.
  )
  (depends
    liquidsoap-core
    liquidsoap-stdlib))

(package
  (name liquidsoap-core)
  (depends
    (ocaml (>= 4.12.0))
    (camomile (>= 1.0.0))
    (dtools (>= 0.4.4))
    (duppy (>= 0.9.1))
    (mm (>= 0.8.1))
    (pcre (>= 7.4.0))
    menhirLib
    dune-site
    dune-build-info
    liquidsoap-lang
    (ppx_string :build))
  (sites (share libs) (share bin) (lib_root lib_root))
  (synopsis "Liquidsoap core module"))

(package
  (name liquidsoap-lang)
  (depends
    (ocaml (>= 4.12.0))
    (sedlex (>= 2.2))
    (menhir (>= 20180703)))
  (synopsis "Liquidsoap language"))

(package
  (name liquidsoap-stdlib)
  (synopsis "Liquidsoap standard library")
  (depends liquidsoap-core))

(package
 (name metadata)
 (synopsis "Read metadata from various file formats")
 (description "A pure OCaml library for reading files from various formats.")
 (depends
  camomile))
