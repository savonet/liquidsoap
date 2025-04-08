open Mm_audio

let () =
  let fname = Sys.argv.(1) in
  let f = new Audio.IO.Reader.of_wav_file fname in
  let oss = new Mm_oss.writer f#channels f#sample_rate in
  let blen = 1024 in
  let buf = Audio.create f#channels blen in
  Printf.printf "Opened WAV file with %d channels at %dHz.\n%!" f#channels
    f#sample_rate;
  let _ = Audio.Effect.delay f#channels f#sample_rate 0.2 ~ping_pong:true 0.5 in
  let _ =
    new Audio.Effect.biquad_filter f#channels f#sample_rate `High_pass 400. 1.
  in
  let loop = ref true in
  while !loop do
    let r = f#read buf 0 blen in
    loop := r <> 0;
    (* delay#process buf 0 r; *)
    (* bqf#process buf 0 r; *)
    oss#write buf 0 r
  done;
  oss#close;
  f#close
