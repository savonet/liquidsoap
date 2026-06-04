let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n (name test)\n (modules test)\n (libraries soundtouch))\n\n"
