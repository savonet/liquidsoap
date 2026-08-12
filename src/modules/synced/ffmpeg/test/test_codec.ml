(* Codec-level assertions that need no media file. *)

let test_capabilities () =
  let codec = Avcodec.Audio.find_encoder `Aac in
  let caps = Avcodec.capabilities codec in
  Test_assert.checkf (caps <> []) "aac encoder reports %d capabilities"
    (List.length caps);
  (* Matching a literal is the point: the returned values must be real
     variants, not the raw table entries. Every encoder ffmpeg ships sets
     AV_CODEC_CAP_DR1. *)
  Test_assert.check "aac encoder capabilities contain `Dr1" (List.mem `Dr1 caps)

let test_codec_id_round_trip () =
  let round_trip what string_of_id get_id find_decoder id =
    Test_assert.checkf
      (string_of_id (get_id (find_decoder id)) = string_of_id id)
      "%s decoder id round-trips as %S" what (string_of_id id)
  in
  Avcodec.Audio.(round_trip "audio" string_of_id get_id find_decoder `Aac);
  Avcodec.Video.(round_trip "video" string_of_id get_id find_decoder `H264);
  Avcodec.Subtitle.(
    round_trip "subtitle" string_of_id get_id find_decoder `Subrip)

let () =
  test_capabilities ();
  test_codec_id_round_trip ();
  Test_assert.finish ()
