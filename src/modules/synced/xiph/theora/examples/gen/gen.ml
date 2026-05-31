let executable name libraries =
  Printf.printf "(executable\n (name %s)\n (libraries %s))\n\n" name
    (String.concat " " libraries)

let () = if Has_theora.available then executable "thtranscode" ["theora"]
