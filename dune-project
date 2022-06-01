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
  (depopts
    alsa
    ao
    bjack
    camlimages
    cry
    dssi
    faad
    fdkaac
    ffmpeg
    flac
    frei0r
    gd
    graphics
    gstreamer
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
    ocurl
    ogg
    opus
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
    sdl-liquidsoap
    tsdl
    tsdl-image
    tsdl-ttf
    vorbis
    xmlplaylist)
 (conflicts
    (alsa (< 0.3.0))
    (ao (< 0.2.0))
    (bjack (< 0.1.3))
    (cry (< 0.6.5))
    (dssi (< 0.1.3))
    (faad (< 0.5.0))
    (fdkaac (< 0.3.1))
    (ffmpeg (< 1.1.4))
    (ffmpeg-avutil (< 1.1.4))
    (flac (< 0.3.0))
    (frei0r (< 0.1.0))
    (gstreamer (< 0.3.1))
    (inotify (< 1.0))
    (ladspa (< 0.2.0))
    (lame (< 0.3.5))
    (lastfm (< 0.3.0))
    (lo (< 0.2.0))
    (mad (< 0.5.0))
    (magic (< 0.6))
    (ocurl (< 0.9.2))
    (ogg (< 0.7.0))
    (opus (< 0.2.0))
    (portaudio (< 0.2.0))
    (pulseaudio (< 0.1.4))
    (samplerate (< 0.1.5))
    (shine (< 0.2.0))
    (soundtouch (< 0.1.9))
    (speex (< 0.4.0))
    (srt (< 0.2.1))
    (ssl (< 0.5.2))
    (taglib (< 0.3.7))
    (sdl-liquidsoap (< 2))
    (tsdl-image (< 0.2.0))
    (theora (< 0.4.0))
    (vorbis (< 0.8.0))
    (xmlplaylist (< 0.1.3)))
  (sites (share libs) (share bin) (lib_root lib_root))
  (synopsis "Liquidsoap core module"))

(package
  (name liquidsoap-lang)
  (depends
    (ocaml (>= 4.12.0))
    (sedlex (>= 2.2))
    (menhir (>= 20180703)))
  (depopts pcre)
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
