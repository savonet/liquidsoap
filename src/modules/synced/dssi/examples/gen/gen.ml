let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n\
      \ (name inspect)\n\
      \ (modules inspect)\n\
      \ (libraries dssi unix))\n\n\
       (rule\n\
      \ (alias runtest)\n\
      \ (action\n\
      \  (run ./inspect.exe)))\n\n"
