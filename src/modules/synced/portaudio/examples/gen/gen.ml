let () =
  if Sys.argv.(1) = "true" then (
    Printf.printf
      "(executable\n\
      \ (name noise)\n\
      \ (modules noise)\n\
      \ (libraries portaudio))\n\n";
    Printf.printf
      "(executable\n\
      \ (name list_devices)\n\
      \ (modules list_devices)\n\
      \ (libraries portaudio))\n\n")
