let executable name modules libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name modules
    (String.concat " " libraries)

let () =
  if Has_opus.available then begin
    executable "opus2wav" "opus2wav" ["opus"];
    executable "wav2opus" "wav2opus" ["opus"]
  end
