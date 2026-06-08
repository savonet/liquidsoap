let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(test\n\
      \ (name frei0r_test)\n\
      \ (modules frei0r_test)\n\
      \ (libraries frei0r unix))\n\n"
