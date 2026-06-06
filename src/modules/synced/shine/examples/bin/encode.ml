let src = ref ""
let dst = ref ""
let buflen = ref 1024

let input_string chan len =
  let ans = Buffer.create len in
  let buf = Bytes.create len in
  let rec f rem =
    if 0 < rem then (
      let ret = input chan buf 0 rem in
      Buffer.add_subbytes ans buf 0 ret;
      f (rem - ret))
  in
  f len;
  Buffer.contents ans

let input_int chan =
  let buf = input_string chan 4 in
  int_of_char buf.[0]
  + (int_of_char buf.[1] lsl 8)
  + (int_of_char buf.[2] lsl 16)
  + (int_of_char buf.[3] lsl 24)

let input_short chan =
  let buf = input_string chan 2 in
  int_of_char buf.[0] + (int_of_char buf.[1] lsl 8)

let bitrate = ref 128
let usage = "usage: encode [options] source destination"

let _ =
  Arg.parse
    [("--bitrate", Arg.Int (fun b -> bitrate := b), "Encoding bitrate.")]
    (let pnum = ref (-1) in
     fun s ->
       incr pnum;
       match !pnum with
         | 0 -> src := s
         | 1 -> dst := s
         | _ ->
             Printf.eprintf "Error: too many arguments\n";
             exit 1)
    usage;
  if !src = "" || !dst = "" then (
    Printf.printf "%s\n" usage;
    exit 1);
  let ic = open_in_bin !src in
  (* TODO: improve! *)
  if input_string ic 4 <> "RIFF" then invalid_arg "No RIFF tag";
  ignore (input_string ic 4);
  if input_string ic 4 <> "WAVE" then invalid_arg "No WAVE tag";
  if input_string ic 4 <> "fmt " then invalid_arg "No fmt tag";
  let _ = input_int ic in
  let _ = input_short ic in
  (* TODO: should be 1 *)
  let channels = input_short ic in
  let infreq = input_int ic in
  let _ = input_int ic in
  (* bytes / s *)
  let _ = input_short ic in
  (* block align *)
  let bits = input_short ic in
  if bits <> 16 then failwith "only s16le is supported for now..";
  let oc = open_out !dst in
  let params = { Shine.channels; samplerate = infreq; bitrate = !bitrate } in
  let enc = Shine.create params in
  let start = Unix.time () in
  Printf.printf "Input detected: PCM WAVE %d channels, %d Hz, %d bits\n%!"
    channels infreq bits;
  Printf.printf
    "Encoding to: MP3 %d channels, %d Hz, bitrate: %d\nPlease wait...\n%!"
    channels infreq !bitrate;
  let rec f () =
    let tag = input_string ic 4 in
    let len = input_int ic in
    if tag <> "data" then (
      ignore (input_string ic len);
      f ())
  in
  f ();
  (* This ensures the actual audio data will start on a new page, as per
   * spec. *)
  let buflen = 2 * channels * Shine.samples_per_pass enc in
  let buf = Bytes.create buflen in
  begin try
    while true do
      really_input ic buf 0 (Bytes.length buf);
      output_string oc (Shine.encode_s16le enc (Bytes.to_string buf) channels)
    done
  with End_of_file -> ()
  end;
  output_string oc (Shine.flush enc);
  close_in ic;
  Printf.printf "Finished in %.0f seconds.\n" (Unix.time () -. start);
  Gc.full_major ()
