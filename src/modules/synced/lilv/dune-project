(lang dune 3.23)
(version 0.2.1)
(name lilv)
(source (github savonet/ocaml-lilv))
(license LGPL-2.1)
(authors "The Savonet Team <contact@liquidsoap.info>")
(maintainers "The Savonet Team <contact@liquidsoap.info>")

(generate_opam_files true)
(subst disabled)
(use_standard_c_and_cxx_flags false)
(using ctypes 0.3)

(package
 (name lilv)
 (synopsis "Bindings to lilv library for using LV2 audio plugins")
 (description "Bindings to the lilv library for simple use of LV2 plugins in applications. LV2 is an open extensible standard for audio plugins. Many types of plugins can be built with LV2, including audio effects, synthesizers, and control processors for modulation and automation.")
 (allow_empty)
 (depends
  conf-pkg-config
  conf-lilv
  dune
  dune-configurator
  ctypes
  ctypes-foreign))
