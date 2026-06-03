let () =
  if Has_soundtouch.available then
    Printf.printf
      "(executable\n (name test)\n (modules test)\n (libraries soundtouch))\n\n"
