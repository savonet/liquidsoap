module S = Soundtouch

let () =
  let chans = 2 in
  let samplerate = 44100 in
  let s = S.make chans samplerate in
  Printf.printf "Sountouch version %s\n\n%!" (S.get_version_string s);
  S.set_rate s 1.2;
  let buflen = 10000 in
  let buf =
    Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout (chans * buflen)
  in
  for _ = 0 to 10 do
    S.put_samples_ba s buf
  done;
  for _ = 0 to 10 do
    ignore (S.get_samples_ba s buf)
  done
