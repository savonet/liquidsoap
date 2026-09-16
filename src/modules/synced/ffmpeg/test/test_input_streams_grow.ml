(* Demuxers without a header, such as MPEG-PS, add streams as they find them
   while reading. The stream contexts must follow, or closing the input walks
   past the end of them. *)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s input_file\n" Sys.argv.(0);
    exit 1);

  let input_file = Sys.argv.(1) in
  let src = Av.open_input input_file in
  let streams_at_open = List.length (Av.get_audio_streams src) in
  let _, stream, _ = Av.find_best_audio_stream src in

  let rec read_all () =
    match Av.read_input ~audio_frame:[stream] src with
      | _ -> read_all ()
      | exception Avutil.Error `Eof -> ()
  in
  read_all ();

  let streams_at_end = List.length (Av.get_audio_streams src) in
  Test_assert.checkf
    (streams_at_open < streams_at_end)
    "streams were added while reading (%d at open, %d at end)" streams_at_open
    streams_at_end;

  Av.close src;
  Test_assert.finish ()
