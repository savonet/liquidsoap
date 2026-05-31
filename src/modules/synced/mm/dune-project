(lang dune 3.23)
(version 0.8.7)
(name mm)
(source (github savonet/ocaml-mm))
(license LGPL-2.1-or-later)
(authors "Samuel Mimram <smimram@gmail.com>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name mm)
 (synopsis "The mm library contains high-level APIs to create and manipulate multimedia streams (audio, video, MIDI)")
 (depends
  (ocaml (>= 4.12))
  dune
  dune-configurator)
 (depopts
  alsa
  ao
  mad
  pulseaudio
  ocamlsdl
  theora)
(conflicts
  (mad (< 0.5.0))
  (alsa (< 0.3.0))))
