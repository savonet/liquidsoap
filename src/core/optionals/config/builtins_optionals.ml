let liquidsoap_build_config_optionals =
  Lang.add_module ~base:Liquidsoap_lang.Builtins_lang.liquidsoap_build_config
    "optionals"

let () =
  List.iter
    (fun (name, value) ->
      ignore
        (Lang.add_builtin_base ~category:`Configuration
           ~descr:("Build-time configuration for " ^ name)
           ~base:liquidsoap_build_config_optionals name (`Bool value)
           Lang.bool_t))
    [
      ("alsa", Alsa_option.enabled);
      ("ao", Ao_option.enabled);
      ("bjack", Bjack_option.enabled);
      ("camlimages", Camlimages_option.enabled);
      ("dssi", Dssi_option.enabled);
      ("faad", Faad_option.enabled);
      ("fdkaac", Fdkaac_option.enabled);
      ("ffmpeg", Ffmpeg_option.enabled);
      ("flac", Flac_option.enabled);
      ("frei0r", Frei0r_option.enabled);
      ("gd", Gd_option.enabled);
      ("graphics", Graphics_option.enabled);
      ("inotify", Inotify_option.enabled);
      ("irc", Irc_option.enabled);
      ("ladspa", Ladspa_option.enabled);
      ("lame", Lame_option.enabled);
      ("lilv", Lilv_option.enabled);
      ("lo", Lo_option.enabled);
      ("mad", Mad_option.enabled);
      ("memtrace", Memtrace_option.enabled);
      ("ogg", Ogg_option.enabled);
      ("opus", Opus_option.enabled);
      ("osc", Osc_option.enabled);
      ("oss", Oss_option.enabled);
      ("portaudio", Portaudio_option.enabled);
      ("posix_time2", Posix_time_option.enabled);
      ("prometheus", Prometheus_option.enabled);
      ("pulseaudio", Pulseaudio_option.enabled);
      ("samplerate", Samplerate_option.enabled);
      ("sdl", Sdl_option.enabled);
      ("shine", Shine_option.enabled);
      ("soundtouch", Soundtouch_option.enabled);
      ("speex", Speex_option.enabled);
      ("srt", Srt_option.enabled);
      ("ssl", Ssl_option.enabled);
      ("tls", Tls_option.enabled);
      ("theora", Theora_option.enabled);
      ("vorbis", Vorbis_option.enabled);
      ("winsvc", Winsvc_option.enabled);
      ("xmlplaylist", Xmlplaylist_option.enabled);
    ]
