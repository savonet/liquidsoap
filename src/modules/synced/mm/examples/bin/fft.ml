open Mm_audio
module FFT = Audio.Mono.Analyze.FFT

let () =
  let fname = Sys.argv.(1) in
  let f = new Audio.IO.Reader.of_wav_file fname in
  let oss = new Mm_oss.writer f#channels f#sample_rate in
  Printf.printf "Opened WAV file with %d channels at %dHz.\n%!" f#channels
    f#sample_rate;
  let fft_n = 11 in
  let fft = FFT.init fft_n in
  let fft_times_per_buf = 4 in
  let blen = 1 lsl fft_n in
  let buf = Audio.create f#channels (2 * blen) in
  let loop = ref true in
  Graphics.open_graph "";
  let i = ref 0 in
  while !loop do
    Audio.blit buf blen buf 0 blen;
    let n = f#read buf blen blen in
    oss#write buf blen n;
    for o = 0 to fft_times_per_buf - 1 do
      let c =
        FFT.complex_create (Audio.to_mono buf 0 blen)
          (o * blen / fft_times_per_buf)
          blen
      in
      FFT.Window.cosine c;
      FFT.fft fft c;
      for j = 0 to Graphics.size_y () - 1 do
        let v = Complex.norm c.(j * blen / (2 * Graphics.size_y ())) in
        let v = int_of_float (v *. 255.) in
        let color = v lsl 16 in
        Graphics.set_color color;
        Graphics.plot !i j
      done;
      incr i;
      if !i >= Graphics.size_x () then i := 0
    done;
    if n = 0 then loop := false
  done;
  oss#close;
  f#close
