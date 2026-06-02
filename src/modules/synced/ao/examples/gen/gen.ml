let () =
  if Has_ao.available then
    Printf.printf
      "(executable\n (name ao_test)\n (modules ao_test)\n (libraries ao))\n\n"
