let () =
  ignore (Dtools.Init.make ~after:[Ffmpeg_utils.log_start_atom] Avdevice.init)
