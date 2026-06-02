let () =
  let exe = Sys.argv.(1) in
  let modules = Sys.argv.(2) in
  let libs =
    Array.sub Sys.argv 3 (Array.length Sys.argv - 3)
    |> Array.to_list |> String.concat " "
  in
  if Has_alsa.available then
    Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n"
      exe modules libs
