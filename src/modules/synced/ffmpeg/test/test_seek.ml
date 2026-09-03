(* A seek leaves the stream decoders holding frames from before the seek
   point. Handed back as-is, they carry pre-seek timestamps and look to the
   caller like the new position, which is enough to make a rate filter
   downstream fabricate the whole skipped interval. *)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s input_file\n" Sys.argv.(0);
    exit 1);

  let input_file = Sys.argv.(1) in
  let src = Av.open_input input_file in
  let _, stream, _ = Av.find_best_video_stream src in
  let { Avutil.num; den } = Av.get_time_base stream in
  let seconds_of_pts pts = Int64.to_float pts *. float num /. float den in

  let read_video () =
    let rec f () =
      match Av.read_input ~video_frame:[stream] src with
        | `Video_frame (_, frame) -> Avutil.Frame.pts frame
        | _ -> f ()
    in
    f ()
  in

  (* Enough frames for the decoder to have buffered some of its own. *)
  for _ = 1 to 10 do
    ignore (read_video ())
  done;

  let target = 20. in
  Av.seek ~fmt:`Millisecond ~ts:(Int64.of_float (target *. 1000.)) src;

  (match read_video () with
    | None -> Test_assert.check "first frame after seek has a timestamp" false
    | Some pts ->
        let pos = seconds_of_pts pts in
        Test_assert.checkf
          (target -. 1. <= pos)
          "first frame after a %.0fs seek is at %.3fs, not before it" target pos);

  Av.close src;
  Test_assert.finish ()
