(lang dune 3.2)
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
    (ocaml (>= 4.12.0))
    (dtools (>= 0.4.4))
    (duppy (>= 0.9.1))
    (mm (>= 0.8.2))
    (pcre (>= 7.5.0))
    (ocurl (>= 0.9.2))
    (cry (>= 0.6.5))
    uri
    fileutils
    menhirLib
    metadata
    dune-build-info
    liquidsoap-lang
    liquidsoap-libs
    (ppx_string :build))
  (depopts
    alsa
    ao
    bjack
    camlimages
    camomile
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
    osx-secure-transport
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
    theora
    tsdl
    tsdl-image
    tsdl-ttf
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
    (ffmpeg (< 1.1.5))
    (ffmpeg-avutil (< 1.1.5))
    (flac (< 0.3.0))
    (frei0r (< 0.1.0))
    (gstreamer (< 0.3.1))
    (inotify (< 1.0))
    (ladspa (< 0.2.0))
    (lame (< 0.3.5))
    (lastfm (< 0.3.0))
    (lo (< 0.2.0))
    (liquidsoap (< 2.2.0))
    (mad (< 0.5.0))
    (magic (< 0.6))
    (mem_usage (< 0.0.3))
    (ogg (< 0.7.0))
    (opus (< 0.2.0))
    (portaudio (< 0.2.0))
    (posix-time2 (< 2.0.2))
    (pulseaudio (< 0.1.4))
    (samplerate (< 0.1.5))
    (shine (< 0.2.0))
    (soundtouch (< 0.1.9))
    (speex (< 0.4.0))
    (srt (< 0.3.0))
    (ssl (< 0.5.2))
    (taglib (< 0.3.10))
    (sdl-liquidsoap (< 2))
    (tsdl-image (< 0.3.2))
    (theora (< 0.4.0))
    (vorbis (< 0.8.0))
    (xmlplaylist (< 0.1.3)))
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
    (ocaml (>= 4.12.0))
    dune-site
    (ppx_string :build)
    (sedlex (>= 2.2))
    (menhir (>= 20180703)))
  (conflicts
    (liquidsoap (<> :version)))
  (sites (share libs) (share bin) (lib_root lib_root))
  (synopsis "Liquidsoap language"))

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
   (name liquidsoap-libs)
   (depends (liquidsoap-lang :build))
   (conflicts
     (liquidsoap-libs (<> :version)))
   (synopsis "Liquidosap standard library"))

(package
   (name liquidsoap-mode)
   (depends (liquidsoap-lang :build) (liquidsoap-libs :build))
   (conflicts
     (liquidsoap-lang (<> :version)))
   (synopsis "Liquidosap emacs mode")
)
