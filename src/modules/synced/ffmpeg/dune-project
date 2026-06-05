(lang dune 3.23)

(name ffmpeg)
(version 1.4.0)
(source (github savonet/ocaml-ffmpeg))
(license LGPL-2.1-only)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "Romain Beauxis <toots@rastageeks.org>")

(generate_opam_files true)
(opam_file_location inside_opam_directory)
(subst disabled)
(use_standard_c_and_cxx_flags false)

(package
 (name ffmpeg-avutil)
 (synopsis "Bindings for the ffmpeg avutil libraries")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  base-threads)
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-avcodec)
 (synopsis "Bindings for the ffmpeg avcodec library")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-avutil (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-avfilter)
 (synopsis "Bindings for the ffmpeg avfilter library")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-avutil (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-swscale)
 (synopsis "Bindings for the ffmpeg swscale library")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-avutil (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-swresample)
 (synopsis "Bindings for the ffmpeg swresample library")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-avutil (= :version))
  (ffmpeg-avcodec (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-av)
 (synopsis "Bindings for the ffmpeg libraries -- top-level helpers")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-avutil (= :version))
  (ffmpeg-avcodec (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg-avdevice)
 (synopsis "Bindings for the ffmpeg avdevice library")
 (allow_empty)
 (depends
  (conf-pkg-config :build)
  (conf-ffmpeg :build)
  (ocaml (>= 4.12))
  dune
  (dune-configurator :build)
  (ffmpeg-av (= :version)))
 (conflicts
  (ffmpeg (< 0.5.0)))
)

(package
 (name ffmpeg)
 (synopsis "Bindings for the ffmpeg libraries")
 (allow_empty)
 (depends
  (ocaml (>= 4.12))
  dune
  (ffmpeg-av (= :version))
  (ffmpeg-avutil (= :version))
  (ffmpeg-avcodec (= :version))
  (ffmpeg-avfilter (= :version))
  (ffmpeg-avdevice (= :version))
  (ffmpeg-swscale (= :version))
  (ffmpeg-swresample (= :version)))
)
