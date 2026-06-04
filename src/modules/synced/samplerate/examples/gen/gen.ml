let () =
  if Has_samplerate.available then
    Printf.printf
      "(executable\n\
      \ (name test)\n\
      \ (modules test)\n\
      \ (libraries samplerate))\n\n\
       (rule\n\
      \ (alias citest)\n\
      \ (action (run ./test.exe)))\n\n"
