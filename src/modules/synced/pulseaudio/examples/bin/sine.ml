open Pulseaudio

let freq = 440.

let () =
  let ss =
    {
      sample_format = Sample_format_float32le;
      sample_rate = 44100;
      sample_chans = 2;
    }
  in
  let simple =
    try
      Simple.create ~client_name:"Test" ~dir:Dir_playback ~stream_name:"Sine"
        ~sample:ss ()
    with Error n ->
      Printf.eprintf "Error: %s\n%!" (string_of_error n);
      exit (-1)
  in
  let buflen = 40000 in
  let buf =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout
      (ss.sample_chans * buflen)
  in
  let t = ref 0. in
  while true do
    for i = 0 to buflen - 1 do
      for c = 0 to ss.sample_chans - 1 do
        buf.{(i * ss.sample_chans) + c} <- sin (freq *. !t)
      done;
      t := !t +. (2. *. Float.pi /. float ss.sample_rate)
    done;
    while !t >= 2. *. Float.pi do
      t := !t -. (2. *. Float.pi)
    done;
    Simple.write_ba simple buf
  done;
  Simple.free simple
