(lang dune 3.23)
(version 1.0.0)
(name ogg)
(source (github savonet/ocaml-xiph))
(license LGPL-2.1-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(opam_file_location inside_opam_directory)
(use_standard_c_and_cxx_flags false)

(package
 (name ogg)
 (allow_empty)
 (synopsis "Bindings to libogg")
 (depends
  conf-libogg
  conf-pkg-config
  (ocaml (>= 4.08.0))
  dune
  dune-configurator)
)

(package
 (name vorbis)
 (allow_empty)
 (synopsis "Bindings to libvorbis")
 (depends
  conf-libvorbis
  conf-pkg-config
  (ocaml (>= 4.03.0))
  dune
  dune-configurator
  (ogg (= :version)))
)

(package
 (name speex)
 (allow_empty)
 (synopsis "Bindings to libspeex")
 (depends
  conf-libogg
  conf-libspeex
  conf-pkg-config
  dune
  dune-configurator
  (ocaml (>= 4.07))
  (ogg (= :version)))
)

(package
 (name theora)
 (allow_empty)
 (synopsis "Bindings to libtheora")
 (depends
  conf-libtheora
  conf-pkg-config
  dune
  dune-configurator
  (ogg (= :version)))
)

(package
 (name opus)
 (allow_empty)
 (synopsis "Bindings to libopus")
 (depends
  conf-libogg
  conf-libopus
  conf-pkg-config
  (ocaml (>= 4.08.0))
  dune
  dune-configurator
  (ogg (= :version)))
)

(package
 (name flac)
 (allow_empty)
 (synopsis "Bindings to libflac")
 (depends
  conf-libflac
  conf-pkg-config
  (ocaml (>= 4.03.0))
  dune
  dune-configurator)
 (depopts
  (ogg (= :version)))
)
