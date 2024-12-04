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

(version 2.3.1)
(generate_opam_files true)
(executables_implicit_empty_intf true)

(package
  (name liquidsoap)
  (depends
    (liquidsoap-core (= :version))
    (liquidsoap-libs (and (>= 2.3.1) (< 2.3.2)))
    (liquidsoap-libs-extra (and (>= 2.3.1) (< 2.3.2)))
    (pandoc :with-doc)
    (pandoc-include :with-doc))
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
   (depends
      (liquidsoap-libs (= :version))
      (liquidsoap-lang :build))
   (synopsis "Liquidosap standard library -- extra functionalities"))

(package
   (name liquidsoap-libs)
   (depends (liquidsoap-lang :build))
   (synopsis "Liquidosap standard library"))

(package
  (name liquidsoap-core)
  (depends
    (ocaml (>= 4.14))
    (dtools (>= 0.4.5))
    (duppy (>= 0.9.4))
    (mm (>= 0.8.4))
    (re (>= 1.11.0))
    (ocurl (>= 0.9.2))
    (cry (>= 1.0.3))
    (camomile (>= 2.0.0))
    uri
    fileutils
    menhirLib
    (mem_usage (>= 0.1.1))
    (metadata (>= 0.3.0))
    magic-mime
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
    memtrace
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
    sqlite3
    srt
    ssl
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
    (ffmpeg (< 1.2.0))
    (ffmpeg-avutil (< 1.2.0))
    (flac (< 0.3.0))
    (frei0r (< 0.1.0))
    (inotify (< 1.0))
    (ladspa (< 0.2.0))
    (lame (< 0.3.7))
    (lastfm (< 0.3.4))
    (lo (< 0.2.0))
    (mad (< 0.5.0))
    (magic (< 0.6))
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
    (ssl (< 0.7.0))
    (tls (< 1.0.2))
    (sdl-liquidsoap (< 2))
    (theora (< 0.4.0))
    (vorbis (< 0.8.0))
    (xmlplaylist (< 0.1.3)))
  (synopsis "Liquidsoap core library and binary"))

(package
  (name liquidsoap-lang)
  (depends
    (ocaml (>= 4.14))
    dune-site
    (saturn_lockfree (and (>= 0.4.1) (< 0.5.0)))
    (re (>= 1.11.0))
    (ppx_string :build)
    (ppx_hash :build)
    (sedlex (>= 3.2))
    (menhir (>= 20240715))
  )
  (sites (share libs) (share bin) (share cache) (lib_root lib_root))
  (synopsis "Liquidsoap language library"))

(package
  (name liquidsoap-js)
  (depends
    (ocaml (>= 4.14))
    (liquidsoap-lang (= :version))
    js_of_ocaml-ppx
    (js_of_ocaml (>= 5.7.2)))
  (conflicts
    (liquidsoap (<> :version)))
  (synopsis "Liquidsoap language - javascript wrapper"))

(package
   (name liquidsoap-mode)
   (depends (liquidsoap (= :version)))
   (synopsis "Liquidosap emacs mode")
)

(package
   (name tls-liquidsoap)
   (version 1)
   (allow_empty)
   (depends tls ca-certs mirage-crypto-rng cstruct)
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
