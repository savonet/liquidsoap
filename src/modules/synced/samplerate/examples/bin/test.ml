(* Test regular interface *)
let () =
  let samples = 256 in
  let chans = 2 in
  let off = 11 in
  let buflen = (off + samples) * chans in
  let inbuf = Array.make buflen 0. in
  let outbuf = Array.make (buflen * 2) 0. in
  let conv = Samplerate.create Samplerate.Conv_linear chans in
  for _ = 1 to 10 do
    let i, o =
      Samplerate.process conv 2. inbuf off samples outbuf off (2 * samples)
    in
    Printf.printf "process: %d -> %d\n%!" i o
  done;
  print_newline ()

(* Test bigarray interface *)
let () =
  let buflen = 1024 in
  let buf = Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout buflen in
  let conv = Samplerate.create Samplerate.Conv_linear 1 in
  let outbuf =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (2 * buflen)
  in
  let i, _ = Samplerate.process_ba conv 2. buf outbuf in
  Printf.printf "Converted %d out of %d.\n%!" i buflen
