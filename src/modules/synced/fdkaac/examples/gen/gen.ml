let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name wav2aac)\n\
      \ (modules wav2aac)\n\
      \ (libraries unix fdkaac))\n\n"
