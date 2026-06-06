let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name wav2mp3)\n\
      \ (modules wav2mp3)\n\
      \ (libraries unix lame))\n\n"
