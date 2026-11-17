(* Conformance of Av.codec_attr on avcC extradata and codec tags:
   spec/tests.md §3, requirements 3.29 and 3.30. *)

open Harness

external params_with_extradata :
  'media Avcodec.params -> string -> 'media Avcodec.params
  = "test_params_with_extradata"

external hevc_params : int -> [ `Video ] Avcodec.params = "test_hevc_params"

let fourcc tag =
  String.fold_right (fun c tag -> (tag lsl 8) lor Char.code c) tag 0

let attribute_of_params ?codec_tag ?(name = "avcc.mkv") params =
  let output = Av.open_output (Test_av.file name) in
  let attribute =
    Av.codec_attr (Av.new_stream_copy ?codec_tag ~params output)
  in
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

let requirement_3_30 () =
  let path = Test_av.file "tagged.mp4" in
  let output = Av.open_output path in
  let stream = Test_av.video_stream ~codec:"libx264" ~bindings:[] output in
  for index = 0 to 9 do
    Av.write_frame stream (Test_av.picture index)
  done;
  Av.close output;
  let input = Av.open_input path in
  let _, _, params = List.hd (Av.get_video_streams input) in
  Av.close input;
  let suffix attribute =
    Option.map (fun a -> String.sub a 4 (String.length a - 4)) attribute
  in
  let avc1 = attribute_of_params params in
  let avc3 = attribute_of_params ~codec_tag:(fourcc "avc3") params in
  check
    (Option.map (fun a -> String.sub a 0 5) avc1 = Some "avc1.")
    "H.264 is avc1 by default";
  check
    (Option.map (fun a -> String.sub a 0 5) avc3 = Some "avc3.")
    "an avc3 tag gives an avc3 codec string";
  equal (suffix avc1) (suffix avc3) "the tag leaves the profile and level";
  let hevc tag =
    attribute_of_params ~name:"hevc.ts" (hevc_params (fourcc tag))
  in
  equal (Some "hvc1.1.4.L93.B01") (hevc "hvc1") "an hvc1 tag";
  equal (Some "hev1.1.4.L93.B01") (hevc "hev1") "an hev1 tag";
  equal None (hevc "HEVC") "the tag of an mpegts source is no sample entry";
  equal None (attribute_of_params ~name:"hevc.ts" (hevc_params 0)) "no tag";
  (* The VPS, SPS and PPS of libx265 for 160x120 yuv420p. Its SPS escapes a
     zero run, so the level is not at a fixed byte. *)
  let main_extradata =
    let hex =
      "0000000140010c01ffff0160000003009000000300000300"
      ^ "1e9598090000000001420101016000000300900000030000"
      ^ "03001ea014207965959a4932bc05a0200000030020000003"
      ^ "032100000000014401c172b46240"
    in
    String.init
      (String.length hex / 2)
      (fun index ->
        Char.chr (int_of_string ("0x" ^ String.sub hex (2 * index) 2)))
  in
  let main tag =
    attribute_of_params ~name:"hevc.ts"
      (params_with_extradata (hevc_params (fourcc tag)) main_extradata)
  in
  (* av_mime_codec_str, from libavformat 62.7.100. *)
  if (Av.avformat_version.major, Av.avformat_version.minor) >= (62, 7) then (
    equal (Some "hvc1.1.6.L30.90") (main "hvc1")
      "the string of FFmpeg's HLS muxer";
    equal (Some "hev1.1.6.L30.90") (main "hev1")
      "the same string with the hev1 sample entry")

let requirements = [("3.29", requirement_3_29); ("3.30", requirement_3_30)]
