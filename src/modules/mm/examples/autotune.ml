open Mm_audio
module FFT = Audio.Mono.Analyze.FFT

let channels = 2
let sample_rate = 44100
let periods = 4

let () =
  let fft = FFT.init 11 in
  let blen = FFT.length fft in
  let alsa_in =
    Mm_alsa.rw channels sample_rate ~capture:true ~buffer_size:(periods * blen)
      ~periods ()
  in
  let alsa_out =
    Mm_alsa.rw channels sample_rate ~playback:true ~blocking:false
      ~buffer_size:(periods * blen) ~periods ()
  in
  let buf = Audio.create channels blen in
  (* let agc = Audio.Effect.auto_gain_control channels sample_rate ~rms_target:2. () in *)
  let _ =
    new Audio.Generator.of_mono (new Audio.Mono.Generator.saw sample_rate 440.)
  in
  let loop = ref true in
  Printf.printf "Using Alsa %s (delay: %d samples).\n" alsa_out#version
    alsa_out#delay;
  (* alsa#prepare; *)
  while !loop do
    (* gen#fill buf 0 blen; *)
    (try
       assert (alsa_out#wait 1000);
       let w = alsa_out#write buf 0 blen in
       Printf.printf "Wrote: %d\n%!" w
     with Alsa.Buffer_xrun as e ->
       alsa_out#recover e;
       ignore (alsa_out#write buf 0 blen));
    let _ = alsa_in#read buf 0 blen in
    (* Printf.printf "Read: %d\n%!" r *) ()
  done;
  alsa_in#close;
  alsa_out#close
