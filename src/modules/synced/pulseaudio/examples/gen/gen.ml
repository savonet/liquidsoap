let () =
  if Sys.argv.(1) = "true" then (
    Printf.printf
      "(executable\n\
      \ (name noise)\n\
      \ (modules noise)\n\
      \ (libraries pulseaudio))\n\n";
    Printf.printf
      "(executable\n (name rec)\n (modules rec)\n (libraries pulseaudio))\n\n";
    Printf.printf
      "(executable\n (name sine)\n (modules sine)\n (libraries pulseaudio))\n\n")
