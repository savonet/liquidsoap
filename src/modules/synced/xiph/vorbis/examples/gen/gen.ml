let executable name modules libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name modules
    (String.concat " " libraries)

let () =
  if Sys.argv.(1) = "true" then begin
    executable "ogg2wav" "ogg2wav" ["vorbis"];
    executable "stream2wav" "stream2wav" ["vorbis"];
    executable "wav2ogg" "wav2ogg" ["vorbis"]
  end
