(lang dune 3.23)
(version 0.3.4)
(name srt)
(source (github savonet/ocaml-srt))
(license GPL-2.0-or-later)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)
(use_standard_c_and_cxx_flags false)

(package
 (name srt)
 (synopsis "Binding for the Secure, Reliable, Transport protocol library")
 (description "\| Secure Reliable Transport (SRT) is an open source transport technology
              "\| that optimizes streaming performance across unpredictable networks, such
              "\| as the Internet.
              "\| This package provides OCaml bindings to the C implementation library.
 )
 (allow_empty)
 (depends
  (conf-srt :build)
  (conf-pkg-config :build)
  (ocaml (>= 4.12))
  (dune-configurator :build)
  ocamlfind
  (ctypes (>= 0.21.1))
  (ctypes-foreign (>= 0.21.1))
  integers
  (posix-socket (>= 3.0.0)))
)
