let check_page (header, body) =
  assert (String.length header >= 27 && String.sub header 0 4 = "OggS");
  let segments = Char.code header.[26] in
  assert (String.length header = 27 + segments);
  let body_length = ref 0 in
  for i = 0 to segments - 1 do
    body_length := !body_length + Char.code header.[27 + i]
  done;
  assert (String.length body = !body_length)

let is_end_of_stream (header, _) = Char.code header.[5] land 4 <> 0

let encode title_length =
  let pages = ref [] in
  let write page = pages := page :: !pages in
  let params =
    {
      Flac.Encoder.channels = 2;
      bits_per_sample = 16;
      sample_rate = 44100;
      compression_level = None;
      total_samples = None;
    }
  in
  let { Flac_ogg.Encoder.encoder; first_pages } =
    Flac_ogg.Encoder.create
      ~comments:[("title", String.make title_length 'A')]
      ~serialno:1234n ~write params
  in
  (* Noise does not compress, which makes page bodies larger than the write
     buffer of the stubs. *)
  let noise =
    Array.init 2 (fun _ -> Array.init 44100 (fun _ -> Random.float 2. -. 1.))
  in
  Flac.Encoder.process encoder noise;
  Flac.Encoder.finish encoder;
  let pages = first_pages @ List.rev !pages in
  List.iter check_page pages;
  assert (is_end_of_stream (List.nth pages (List.length pages - 1)))

let () = List.iter encode [100; 1500; 2600; 3600; 20000]
