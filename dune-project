(lang dune 3.18)
(using menhir 2.1)
(using dune_site 0.1)
(name liquidsoap)
(source (github savonet/liquidsoap))
(license GPL-2.0-or-later)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")
(homepage "https://github.com/savonet/liquidsoap")
(bug_reports "https://github.com/savonet/liquidsoap/issues")
(maintenance_intent "(latest)")
(version 2.4.3)

(generate_opam_files true)
(subst disabled)
(opam_file_location inside_opam_directory)
(executables_implicit_empty_intf true)

(package
  (name liquidsoap)
   (depends
    (ocaml (>= 4.14))
    base-bytes
    (mm (>= 0.8.6))
    (re (and (>= 1.11.0) (< 1.14.0)))
    curl
    (camomile (>= 2.0.0))
    uri
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
    inotify
    irc-client-unix
    jemalloc
    ladspa
    lame
    lilv
    lo
    mad
    memtrace
    ogg
    opus
    osc-unix
    portaudio
    posix-time2
    posix-socket
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
    yaml)
 (conflicts
    (alsa (< 0.3.0))
    (ao (< 0.2.0))
    (bjack (< 0.1.3))
    (camomile (< 1.0.0))
    (dssi (< 0.1.3))
    (faad (< 0.5.0))
    (fdkaac (< 0.3.1))
    (ffmpeg (< 1.3.0))
    (ffmpeg-avutil (< 1.2.5))
    (flac (< 1.0.0))
    (frei0r (< 0.1.0))
    (inotify (< 1.0))
    (ladspa (< 0.2.0))
    (lame (< 0.3.7))
    (lo (< 0.2.0))
    (mad (< 0.5.0))
    (magic (< 0.6))
    (mirage-crypto-rng (< 0.6.2))
    (ogg (< 1.0.0))
    (opus (< 0.2.0))
    (odoc (< 3.0.0~beta1))
    (portaudio (< 0.2.0))
    (posix-time2 (< 2.0.2))
    (posix-socket (< 2.1.0))
    (pulseaudio (< 0.1.4))
    (samplerate (< 0.1.5))
    (shine (< 0.2.0))
    (soundtouch (< 0.1.9))
    (speex (< 1.0.0))
    (srt (< 0.3.2))
    (ssl (< 0.7.0))
    (tls (< 1.0.2))
    (sdl-liquidsoap (< 2))
    (theora (< 1.0.0))
    (vorbis (< 1.0.0))
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
  (name liquidsoap-lang)
  (depends
    (ocaml (>= 4.14))
    dune-site
    (re (>= 1.11.0))
    (ppx_string :build)
    (ppx_hash :build)
    (sedlex (>= 3.2))
    (menhir (>= 20240715))
    fileutils
    xml-light
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
