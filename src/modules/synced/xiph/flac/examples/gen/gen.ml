let has_ffmpeg = Sys.command "which ffmpeg > /dev/null 2>&1" = 0

let () =
  if Sys.argv.(1) = "true" then begin
    print_string
      {|(executable
 (name decode)
 (modules decode)
 (libraries flac.ogg))

(executable
 (name encode)
 (modules encode)
 (libraries flac.ogg))

|};
    if has_ffmpeg then
      print_string
        {|(rule
 (alias xiph_citest)
 (target src.wav)
 (action
  (run
   ffmpeg
   -hide_banner
   -loglevel
   error
   -f
   lavfi
   -i
   "sine=frequency=220:duration=5"
   -ac
   2
   %{target})))

(rule
 (alias xiph_citest)
 (deps ./src.wav)
 (action
  (progn
   (run ./encode.exe ./src.wav ./src.flac)
   (run ./decode.exe -i ./src.flac -o ./dst.wav)
   (run ./encode.exe --ogg true ./src.wav ./dst.ogg)
   (run ./decode.exe -ogg true -i ./dst.ogg -o ./ogg-dst.wav))))
|}
  end
