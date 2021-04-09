let print_config =
  Printf.printf
    {|
Congratulation on building liquidsoap! Here are the details
of your build and configuration:

 * Liquidsoap version  : %s

 * Supported input formats
   - MP3               : %b
   - AAC               : %b
   - Ffmpeg            : %b
   - Flac (native)     : %b
   - Flac (ogg)        : %b
   - Opus              : %b
   - Speex             : %b
   - Theora            : %b
   - Vorbis            : %b

 * Supported output formats
   - FDK-AAC           : %b
   - Ffmpeg            : %b
   - MP3               : %b
   - MP3 (fixed-point) : %b
   - Opus              : %b
   - Speex             : %b
   - Theora            : %b
   - Vorbis            : %b

 * Tags
   - Taglib (ID3 tags) : %b
   - Vorbis            : %b

 * Input / output
   - ALSA              : %b
   - AO                : %b
   - FFmpeg            : %b
   - Icecast/Shoutcast : %b
   - GStreamer         : %b
   - JACK              : %b
   - OSS               : %b
   - Portaudio         : %b
   - Pulseaudio        : %b
   - SRT               : %b

 * Audio manipulation
   - FFmpeg            : %b
   - LADSPA            : %b
   - Lilv              : %b
   - Samplerate        : %b
   - SoundTouch        : %b

 * Video manipulation
   - camlimages        : %b
   - FFmpeg            : %b
   - frei0r            : %b
   - SDL               : %b

 * MIDI manipulation
   - DSSI              : %b

 * Visualization
   - GD                : %b
   - Graphics          : %b
   - SDL               : %b

 * Additional libraries
   - FFmpeg filters:   : %b
   - inotify           : %b
   - lastfm            : %b
   - lo                : %b
   - magic             : %b
   - SecureTransport   : %b
   - ssl               : %b
   - posix-time2       : %b
   - windows service   : %b
   - yojson            : %b
   - XML playlists     : %b

 * Monitoring
   - Prometheus        : %b
|}

let liquidsoap_config =
  Printf.sprintf
    {|
opam-version: "2.0"
variables {
  alsa-enabled: %b
  ao-enabled: %b
  bjack-enabled: %b
  camlimages-enabled: %b
  camomile-enabled: %b
  cry-enabled: %b
  dssi-enabled: %b
  faad-enabled: %b
  fdkaac-enabled: %b
  ffmpeg-enabled: %b
  flac-enabled: %b
  frei0r-enabled: %b
  gd-enabled: %b
  graphics-enabled: %b
  gstreamer-enabled: %b
  inotify-enabled: %b
  ladspa-enabled: %b
  lame-enabled: %b
  lastfm-enabled: %b
  lo-enabled: %b
  mad-enabled: %b
  magic-enabled: %b
  ogg-enabled: %b
  ogg_flac-enabled: %b
  opus-enabled: %b
  portaudio-enabled: %b
  posix-time-enabled: %b
  prometheus-enabled: %b
  pulseaudio-enabled: %b
  samplerate-enabled: %b
  sdl-enabled: %b
  secure_transport-enabled: %b
  shine-enabled: %b
  soundtouch-enabled: %b
  speex-enabled: %b
  srt-enabled: %b
  ssl-enabled: %b
  taglib-enabled: %b
  theora-enabled: %b
  vorbis-enabled: %b
  winscv-enabled: %b
  xmlplaylist-enabled: %b
  yojson-enabled: %b
}
|}

let () =
  print_config (Configure.version ()) Mad_option.enabled Faad_option.enabled
    Ffmpeg_option.enabled Flac_option.enabled Ogg_flac_option.enabled
    Opus_option.enabled Speex_option.enabled Theora_option.enabled
    Vorbis_option.enabled Fdkaac_option.enabled Ffmpeg_option.enabled
    Lame_option.enabled Shine_option.enabled Opus_option.enabled
    Speex_option.enabled Theora_option.enabled Vorbis_option.enabled
    Taglib_option.enabled Vorbis_option.enabled Alsa_option.enabled
    Ao_option.enabled Ffmpeg_option.enabled Cry_option.enabled
    Gstreamer_option.enabled Bjack_option.enabled Oss_option.enabled
    Portaudio_option.enabled Pulseaudio_option.enabled Srt_option.enabled
    Ffmpeg_option.enabled Ladspa_option.enabled Lilv_option.enabled
    Samplerate_option.enabled Soundtouch_option.enabled
    Camlimages_option.enabled Ffmpeg_option.enabled Frei0r_option.enabled
    Sdl_option.enabled Dssi_option.enabled Gd_option.enabled
    Graphics_option.enabled Sdl_option.enabled Ffmpeg_option.enabled
    Inotify_option.enabled Lastfm_option.enabled Lo_option.enabled
    Magic_option.enabled Secure_transport_option.enabled Ssl_option.enabled
    Posix_time_option.enabled Winsvc_option.enabled Yojson_option.enabled
    Xmlplaylist_option.enabled Prometheus_option.enabled;

  let oc = open_out "liquidsoap.config" in
  output_string oc
    (liquidsoap_config Alsa_option.enabled Ao_option.enabled
       Bjack_option.enabled Camlimages_option.enabled Cry_option.enabled
       Dssi_option.enabled Faad_option.enabled Fdkaac_option.enabled
       Ffmpeg_option.enabled Flac_option.enabled Frei0r_option.enabled
       Gd_option.enabled Graphics_option.enabled Gstreamer_option.enabled
       Inotify_option.enabled Ladspa_option.enabled Lame_option.enabled
       Lastfm_option.enabled Lilv_option.enabled Lo_option.enabled
       Mad_option.enabled Magic_option.enabled Ogg_flac_option.enabled
       Opus_option.enabled Oss_option.enabled Portaudio_option.enabled
       Posix_time_option.enabled Prometheus_option.enabled
       Pulseaudio_option.enabled Samplerate_option.enabled Sdl_option.enabled
       Secure_transport_option.enabled Shine_option.enabled
       Soundtouch_option.enabled Speex_option.enabled Srt_option.enabled
       Ssl_option.enabled Taglib_option.enabled Theora_option.enabled
       Vorbis_option.enabled Winsvc_option.enabled Xmlplaylist_option.enabled
       Yojson_option.enabled);
  close_out oc
