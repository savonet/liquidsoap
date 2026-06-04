let executable name libraries =
  Printf.printf "(executable\n (name %s)\n (libraries %s))\n\n" name
    (String.concat " " libraries)

let () = if Sys.argv.(1) = "true" then executable "thtranscode" ["theora"]
