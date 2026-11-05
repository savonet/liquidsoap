(* Conformance of Av.flush_encoder: spec/tests.md §3, requirement 3.28. *)

open Harness

let frames = 30

let requirement_3_28 () =
  let written = Buffer.create 65536 in
  let write bytes offset length =
    Buffer.add_subbytes written bytes offset length;
    length
  in
  let output = Av.open_output_stream write (Test_av.mpegts ()) in
  (* B-frames make the encoder hold pictures until it is drained. *)
  let stream = Test_av.video_stream output in
  let reported = ref 0 in
  let on_packet _ = incr reported in
  for index = 0 to frames - 1 do
    Av.write_frame ~on_packet stream (Test_av.picture index)
  done;
  let by_write_frame = !reported in
  Av.flush_encoder
    ~on_packet:(fun packet ->
      on_packet packet;
      ignore (Av.tell output))
    stream;
  check
    (!reported > by_write_frame)
    "flush_encoder reports the packets the encoder held";
  equal frames !reported "every frame is reported as one packet";
  raises Test_av.is_eof "a frame after the drain" (fun () ->
      Av.write_frame stream (Test_av.picture frames));
  Av.close output;
  let path = Test_av.file "flushed.ts" in
  Out_channel.with_open_bin path (fun channel ->
      Out_channel.output_string channel (Buffer.contents written));
  let input = Av.open_input path in
  equal frames
    (List.length (Test_av.video_frames input))
    "the closed output holds every frame";
  Av.close input

let requirements = [("3.28", requirement_3_28)]
