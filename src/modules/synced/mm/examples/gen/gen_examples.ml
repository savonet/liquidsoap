let executable name libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name name
    (String.concat " " libraries)

let () =
  if Has_alsa.available then executable "autotune" ["mm.audio"; "mm.alsa"];
  if Has_mad.available && Has_oss.available then
    executable "dictee" ["mm.audio"; "mm.midi"; "mm.mad"; "mm.oss"];
  if Has_graphics.available then begin
    executable "display" ["graphics"; "mm.image"];
    executable "rotate" ["graphics"; "mm.image"];
    executable "graphics_test" ["graphics"; "mm"];
    if Has_oss.available then
      executable "fft" ["graphics"; "mm.audio"; "mm.oss"]
  end;
  if Has_oss.available then begin
    executable "drums" ["mm.audio"; "mm.oss"];
    executable "midiplayer" ["mm.audio"; "mm.oss"]
  end;
  executable "id" ["mm.audio"];
  if Has_ao.available then executable "sine_wav" ["mm.audio"; "mm.ao"];
  executable "replaygain" ["mm.audio"];
  executable "test" ["mm"];
  print_string "(rule\n (alias runtest)\n (action\n  (run ./test.exe)))\n"
