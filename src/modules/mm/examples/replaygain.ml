open Mm_audio
module RG = Audio.Analyze.ReplayGain

let () =
  let fname = Sys.argv.(1) in
  Printf.printf "Computing replaygain for %s.\n%!" fname;
  let f = new Audio.IO.Reader.of_wav_file fname in
  let channels = f#channels in
  let rg = RG.create ~channels ~samplerate:f#sample_rate in
  let len = 1024 in
  let buf = Audio.create channels len in
  let total = f#length in
  let processed = ref 0 in
  let loop = ref true in
  while !loop do
    let n = f#read buf 0 len in
    processed := !processed + n;
    Printf.printf "\rProcessing: %d%%%!" (!processed * 100 / total);
    if n = 0 then loop := false;
    RG.process rg buf 0 n
  done;
  Printf.printf "\rProcessing done.\n%!";
  f#close;
  Printf.printf "- peak: %f\n%!" (RG.peak rg);
  Printf.printf "- gain: %f dB\n%!" (RG.gain rg)
