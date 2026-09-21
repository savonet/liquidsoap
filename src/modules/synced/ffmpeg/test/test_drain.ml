(* Frames still held by a decoder when the demuxer runs out must be returned
   before end of input: a single-frame image holds its only frame there. *)

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: %s input_file expected_frames [reseek]\n"
      Sys.argv.(0);
    exit 1);

  let input_file = Sys.argv.(1) in
  let expected = int_of_string Sys.argv.(2) in
  let reseek = Array.length Sys.argv > 3 in
  let src = Av.open_input input_file in
  let _, stream, _ = Av.find_best_video_stream src in

  let count_frames () =
    let rec f count =
      match Av.read_input ~video_frame:[stream] src with
        | `Video_frame _ -> f (count + 1)
        | _ -> f count
        | exception Avutil.Error `Eof -> count
    in
    f 0
  in

  let count = count_frames () in
  Test_assert.checkf (count = expected) "decoded %d frames out of %d" count
    expected;

  if reseek then begin
    Av.seek ~fmt:`Millisecond ~ts:0L src;
    let count = count_frames () in
    Test_assert.checkf (count = expected)
      "decoded %d frames out of %d after seeking back from end of input" count
      expected
  end;

  Av.close src;
  Test_assert.finish ()
