let executable name libraries =
  Printf.printf "(executable\n (name %s)\n (modules %s)\n (libraries %s))\n\n"
    name name
    (String.concat " " libraries)

let () =
  let has_alsa = Sys.argv.(1) = "true" in
  let has_mad = Sys.argv.(2) = "true" in
  let has_oss = Sys.argv.(3) = "true" in
  let has_ao = Sys.argv.(4) = "true" in
  let has_graphics = Sys.argv.(5) = "true" in
  if has_alsa then executable "autotune" ["mm.audio"; "mm.alsa"];
  if has_mad && has_oss then
    executable "dictee" ["mm.audio"; "mm.midi"; "mm.mad"; "mm.oss"];
  if has_graphics then begin
    executable "display" ["graphics"; "mm.image"];
    executable "rotate" ["graphics"; "mm.image"];
    executable "graphics_test" ["graphics"; "mm"];
    if has_oss then executable "fft" ["graphics"; "mm.audio"; "mm.oss"]
  end;
  if has_oss then begin
    executable "drums" ["mm.audio"; "mm.oss"];
    executable "midiplayer" ["mm.audio"; "mm.oss"]
  end;
  executable "id" ["mm.audio"];
  if has_ao then executable "sine_wav" ["mm.audio"; "mm.ao"];
  executable "replaygain" ["mm.audio"];
  executable "test" ["mm"];
  print_string "(rule\n (alias runtest)\n (action\n  (run ./test.exe)))\n"
