(lang dune 3.6)
(using menhir 2.1)
(using dune_site 0.1)
(name liquidsoap)
(source (github savonet/liquidsoap))
(license GPL-2.0-or-later)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")
(homepage "https://github.com/savonet/liquidsoap")
(bug_reports "https://github.com/savonet/liquidsoap/issues")

(version 2.2.0)
(generate_opam_files true)
(executables_implicit_empty_intf true)

(package
  (name liquidsoap)
  (depends
    (liquidsoap-core (= :version))
    (liquidsoap-libs (and (>= 2.2.0~1) (< 2.2.0)))
    (liquidsoap-libs-extra (and (>= 2.2.0~1) (< 2.2.0))))
  (synopsis "Swiss-army knife for multimedia streaming")
  (description
"\| Liquidsoap is a powerful and flexible language for describing your
"\| streams. It offers a rich collection of operators that you can combine
"\| at will, giving you more power than you need for creating or
"\| transforming streams. But liquidsoap is still very light and easy to
"\| use, in the Unix tradition of simple strong components working
"\| together.
  ))

(package
   (name liquidsoap-libs-extra)
   (version 2.2.0~1)
   (depends
      (liquidsoap-libs (= :version))
      (liquidsoap-lang :build))
   (synopsis "Liquidosap standard library -- extra functionalities"))

(package
   (name liquidsoap-libs)
   (version 2.2.0~1)
   (depends (liquidsoap-lang :build))
   (synopsis "Liquidosap standard library"))

(package
  (name liquidsoap-core)
  (depends
    (ocaml (>= 4.14.0))
    (dtools (>= 0.4.5))
    (duppy (>= 0.9.1))
    (mm (>= 0.8.4))
    (pcre (>= 7.5.0))
    (ocurl (>= 0.9.2))
    (cry (>= 1.0.0))
    (camomile (>= 2.0.0))
    uri
    fileutils
    menhirLib
    (metadata (>= 0.2.0))
    dune-build-info
    (liquidsoap-lang (= :version))
    (ppx_string :build))
  (depopts
    alsa
    ao
    bjack
    camlimages
    ctypes-foreign
    dssi
    faad
    fdkaac
    ffmpeg
    flac
    frei0r
    gd
    graphics
    gstreamer
    imagelib
    inotify
    irc-client-unix
    jemalloc
    ladspa
    lame
    lastfm
    lilv
    lo
    mad
    magic
    memtrace
    mem_usage
    ogg
    opus
    osc-unix
    portaudio
    posix-time2
    pulseaudio
    prometheus-liquidsoap
    samplerate
    shine
    soundtouch
    speex
    srt
    ssl
    taglib
    tls-liquidsoap
    theora
    sdl-liquidsoap
    vorbis
    yaml
    xmlplaylist)
 (conflicts
    (alsa (< 0.3.0))
    (ao (< 0.2.0))
    (bjack (< 0.1.3))
    (camomile (< 1.0.0))
    (dssi (< 0.1.3))
    (faad (< 0.5.0))
    (fdkaac (< 0.3.1))
    (ffmpeg (< 1.1.8))
    (ffmpeg-avutil (< 1.1.8))
    (flac (< 0.3.0))
    (frei0r (< 0.1.0))
    (gstreamer (< 0.3.1))
    (inotify (< 1.0))
    (ladspa (< 0.2.0))
    (lame (< 0.3.7))
    (lastfm (< 0.3.0))
    (lo (< 0.2.0))
    (mad (< 0.5.0))
    (magic (< 0.6))
    (mem_usage (< 0.0.3))
    (ogg (< 0.7.4))
    (opus (< 0.2.0))
    (portaudio (< 0.2.0))
    (posix-time2 (< 2.0.2))
    (pulseaudio (< 0.1.4))
    (samplerate (< 0.1.5))
    (shine (< 0.2.0))
    (soundtouch (< 0.1.9))
    (speex (< 0.4.0))
    (srt (< 0.3.0))
    (ssl (< 0.5.13))
    (taglib (< 0.3.10))
    (sdl-liquidsoap (< 2))
    (theora (< 0.4.0))
    (vorbis (< 0.8.0))
    (xmlplaylist (< 0.1.3)))
  (synopsis "Liquidsoap core library and binary"))

(package
  (name liquidsoap-lang)
  (depends
    (ocaml (>= 4.12.0))
    dune-site
    (ppx_string :build)
    (sedlex (>= 3.2))
    (menhir (>= 20180703)))
  (sites (share libs) (share bin) (lib_root lib_root))
  (synopsis "Liquidsoap language library"))

(package
  (name liquidsoap-js)
  (depends
    (ocaml (>= 4.12.0))
    liquidsoap-lang
    js_of_ocaml-ppx
    js_of_ocaml)
  (conflicts
    (liquidsoap (<> :version)))
  (synopsis "Liquidsoap language - javascript wrapper"))

(package
   (name liquidsoap-mode)
   (depends liquidsoap)
   (conflicts
     (liquidsoap-lang (<> :version)))
   (synopsis "Liquidosap emacs mode")
)

(package
   (name tls-liquidsoap)
   (version 1)
   (allow_empty)
   (depends tls ca-certs)
   (synopsis "Virtual package install liquidosap dependencies for TLS optional features")
)

(package
   (name prometheus-liquidsoap)
   (version 2)
   (allow_empty)
   (depends prometheus-app cohttp-lwt-unix)
   (synopsis "Virtual package installing liquidsoap dependencies for prometheus optional features")
)

(package
   (name sdl-liquidsoap)
   (version 3)
   (allow_empty)
   (depends tsdl (tsdl-image (>= 0.3.2)) tsdl-ttf)
   (synopsis "Virtual package installing liquidsoap dependencies for SDL optional features")
)
