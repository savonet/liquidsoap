(* Conformance of Av.codec_attr on avcC extradata: spec/tests.md §3,
   requirement 3.29. *)

open Harness

external params_with_extradata :
  'media Avcodec.params -> string -> 'media Avcodec.params
  = "test_params_with_extradata"

let attribute_of_params params =
  let output = Av.open_output (Test_av.file "avcc.mkv") in
  let attribute = Av.codec_attr (Av.new_stream_copy ~params output) in
  Av.close output;
  attribute

let requirement_3_29 () =
  let path = Test_av.file "avcc.mp4" in
  let output = Av.open_output path in
  let stream = Test_av.video_stream ~codec:"libx264" ~bindings:[] output in
  let annex_b = Av.codec_attr stream in
  for index = 0 to 9 do
    Av.write_frame stream (Test_av.picture index)
  done;
  Av.close output;
  check (annex_b <> None) "the Annex-B extradata of libx264 has a codec string";
  let input = Av.open_input path in
  let _, stream, params = List.hd (Av.get_video_streams input) in
  let extradata = Avcodec.params_extradata params in
  check
    (String.length extradata > 4 && extradata.[0] = '\001')
    "the mp4 demuxer gives an avcC record";
  equal annex_b (Av.codec_attr stream)
    "the avcC record gives the codec string of the Annex-B SPS";
  Av.close input;
  equal annex_b
    (attribute_of_params
       (params_with_extradata params (String.sub extradata 0 4)))
    "the four bytes up to the level are enough";
  equal None
    (attribute_of_params
       (params_with_extradata params (String.sub extradata 0 3)))
    "an avcC record too short for the level gives none"

let requirements = [("3.29", requirement_3_29)]
