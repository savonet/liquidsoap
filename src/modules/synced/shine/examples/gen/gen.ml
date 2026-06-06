let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name encode)\n\
      \ (modules encode)\n\
      \ (libraries shine unix))\n\n"
