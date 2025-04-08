external is_big_endian : unit -> bool = "ocaml_mm_is_big_endian"

let () =
  Printf.printf
    {|
Big endian host: %b

Supported external libraries:
  - Alsa      : %b
  - AO        : %b
  - Mad       : %b
  - OSS       : %b
  - Pulseaudio: %b
  - SDL       : %b
  - Theora    : %b
|}
    (is_big_endian ()) Setup_alsa.is_set Setup_ao.is_set Setup_mad.is_set
    Setup_oss.is_set Setup_pulseaudio.is_set Setup_sdl.is_set
    Setup_theora.is_set;
  let oc = open_out "config.print" in
  close_out oc
