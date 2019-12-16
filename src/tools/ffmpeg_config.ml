let log = Log.make ["ffmpeg"]

let conf_ffmpeg =
  Dtools.Conf.void ~p:(Configure.conf#plug "ffmpeg") "FFMPEG configuration"

let conf_log = Dtools.Conf.void ~p:(conf_ffmpeg#plug "log") "Log configuration"

let conf_verbosity =
  Dtools.Conf.string
    ~p:(conf_log#plug "verbosity")
    "Verbosity" ~d:"quiet"
    ~comments:
      [
        "Set FFMPEG log level, one of: \"quiet\", \"panic\", \"fatal\"";
        "\"error\", \"warning\", \"info\", \"verbose\" or \"debug\"";
      ]

let conf_level = Dtools.Conf.int ~p:(conf_log#plug "level") "Level" ~d:5

let () =
  ignore
    (Dtools.Init.at_start (fun () ->
         let verbosity =
           match conf_verbosity#get with
             | "quiet" -> `Quiet
             | "panic" -> `Panic
             | "fatal" -> `Fatal
             | "error" -> `Error
             | "warning" -> `Warning
             | "info" -> `Info
             | "verbose" -> `Verbose
             | "debug" -> `Debug
             | _ ->
                 log#severe "Invalid value for \"ffmpeg.log.verbosity\"!";
                 `Quiet
         in
         let level = conf_level#get in
         FFmpeg.Avutil.Log.set_level verbosity;
         FFmpeg.Avutil.Log.set_callback (fun s ->
             log#f level "%s" (String.trim s))))
