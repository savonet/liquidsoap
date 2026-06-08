let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name aac2wav)\n\
      \ (modules aac2wav)\n\
      \ (libraries unix faad))\n\n"
