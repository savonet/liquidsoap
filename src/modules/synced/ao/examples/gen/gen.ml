let () =
  if Sys.argv.(1) = "true" then
    Printf.printf
      "(executable\n (name ao_test)\n (modules ao_test)\n (libraries ao))\n\n"
