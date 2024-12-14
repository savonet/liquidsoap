open Liquidsoap_lang

let deprecated_features = []

let build_config =
  let path_mode =
    match Liquidsoap_paths.mode with
      | `Default -> "default"
      | `Standalone -> "standalone"
      | `Posix -> "posix"
  in
  let deprecated_features =
    List.filter_map
      (fun (name, enabled) -> if enabled then Some ("   - " ^ name) else None)
      deprecated_features
  in
  let deprecated_features =
    if deprecated_features <> [] then
      Printf.sprintf "\n * Deprecated features:\n%s\n"
        (String.concat "\n" deprecated_features)
    else ""
  in
  [%string
    {|
 * Liquidsoap version  : %{Build_config.version}
%{deprecated_features}
 * Compilation options
   - Release build       : %{string_of_bool (not Build_config.is_snapshot)}
   - Git SHA             : %{Option.value ~default:"(none)" Build_config.git_sha}
   - OCaml version       : %{Sys.ocaml_version}
   - OS type             : %{Sys.os_type}
   - Libs versions       : %{Configure.libs_versions ()}
   - architecture        : %{Build_config.architecture}
   - host                : %{Build_config.host}
   - target              : %{Build_config.target}
   - system              : %{Build_config.system}
   - ocamlopt_cflags     : %{Build_config.ocamlopt_cflags}
   - native_c_compiler   : %{Build_config.native_c_compiler}
   - native_c_libraries  : %{Build_config.native_c_libraries}

 * Configured paths
   - mode              : %{path_mode}
   - standard library  : %{Liquidsoap_paths.liq_libs_dir_descr}
   - scripted binaries : %{Liquidsoap_paths.bin_dir_descr}
   - rundir            : %{Liquidsoap_paths.rundir_descr}
   - logdir            : %{Liquidsoap_paths.logdir_descr}
   - user cache        : %{Liquidsoap_paths.user_cache_override_descr} (override with $LIQ_CACHE_USER_DIR)
   - system cache      : %{Liquidsoap_paths.system_cache_override_descr} (override with $LIQ_CACHE_SYSTEM_DIR)
   - camomile files    : %{Liquidsoap_paths.camomile_dir_descr}

 * Supported input formats
   - MP3               : %{Mad_option.detected}
   - AAC               : %{Faad_option.detected}
   - Ffmpeg            : %{Ffmpeg_option.detected}
   - Flac (native)     : %{Flac_option.detected}
   - Flac (ogg)        : %{Ogg_flac_option.detected}
   - Opus              : %{Opus_option.detected}
   - Speex             : %{Speex_option.detected}
   - Theora            : %{Theora_option.detected}
   - Vorbis            : %{Vorbis_option.detected}
   - WAV/AIFF          : yes (native)

 * Supported output formats
   - FDK-AAC           : %{Fdkaac_option.detected}
   - FFmpeg            : %{Ffmpeg_option.detected}
   - MP3               : %{Lame_option.detected}
   - MP3 (fixed-point) : %{Shine_option.detected}
   - Flac (native)     : %{Flac_option.detected}
   - Flac (ogg)        : %{Ogg_flac_option.detected}
   - Opus              : %{Opus_option.detected}
   - Speex             : %{Speex_option.detected}
   - Theora            : %{Theora_option.detected}
   - Vorbis            : %{Vorbis_option.detected}
   - WAV/AIFF          : yes (native)

 * Tags
   - AAC               : %{Faad_option.detected}
   - FFmpeg            : %{Ffmpeg_option.detected}
   - FLAC (native)     : %{Flac_option.detected}
   - Flac (ogg)        : %{Ogg_flac_option.detected}
   - Native decoder    : yes
   - Vorbis            : %{Vorbis_option.detected}

 * Input / output
   - ALSA              : %{Alsa_option.detected}
   - AO                : %{Ao_option.detected}
   - FFmpeg            : %{Ffmpeg_option.detected}
   - JACK              : %{Bjack_option.detected}
   - NDI               : %{Ndi_option.detected}
   - OSS               : %{Oss_option.detected}
   - Portaudio         : %{Portaudio_option.detected}
   - Pulseaudio        : %{Pulseaudio_option.detected}
   - SRT               : %{Srt_option.detected}

 * Audio manipulation
   - FFmpeg            : %{Ffmpeg_option.detected}
   - LADSPA            : %{Ladspa_option.detected}
   - Lilv              : %{Lilv_option.detected}
   - Samplerate        : %{Samplerate_option.detected}
   - SoundTouch        : %{Soundtouch_option.detected}
   - StereoTool        : %{Stereotool_option.detected}

 * Video manipulation
   - camlimages        : %{Camlimages_option.detected}
   - FFmpeg            : %{Ffmpeg_option.detected}
   - frei0r            : %{Frei0r_option.detected}
   - ImageLib          : %{Imagelib_option.detected}
   - SDL               : %{Sdl_option.detected}

 * MIDI manipulation
   - DSSI              : %{Dssi_option.detected}

 * Visualization
   - GD                : %{Gd_option.detected}
   - Graphics          : %{Graphics_option.detected}
   - SDL               : %{Sdl_option.detected}

 * Additional libraries
   - FFmpeg filters    : %{Ffmpeg_option.detected}
   - FFmpeg devices    : %{Ffmpeg_option.detected}
   - inotify           : %{Inotify_option.detected}
   - irc               : %{Irc_option.detected}
   - jemalloc          : %{Jemalloc_option.detected}
   - lo                : %{Lo_option.detected}
   - memtrace          : %{Memtrace_option.detected}
   - osc               : %{Osc_option.detected}
   - ssl               : %{Ssl_option.detected}
   - sqlite3           : %{Sqlite3_option.detected}
   - tls               : %{Tls_option.detected}
   - posix-time2       : %{Posix_time_option.detected}
   - windows service   : %{Winsvc_option.detected}
   - YAML support      : %{Yaml_option.detected}
   - XML playlists     : %{Xmlplaylist_option.detected}

 * Monitoring
   - Prometheus        : %{Prometheus_option.detected}
|}]

let opam_config =
  [%string
    {|
opam-version: "2.0"
variables {
  ffmpeg-enabled: %{string_of_bool Ffmpeg_option.enabled}
  ssl-enabled: %{string_of_bool Ssl_option.enabled}
  tls-enabled: %{string_of_bool Tls_option.enabled}
}
|}]
