let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executables\n\
      \ (names osclisten send)\n\
      \ (modules osclisten send)\n\
      \ (libraries lo))\n\n"
