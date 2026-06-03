let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "usage: %s input_file\n" Sys.argv.(0);
    exit 0);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let src = Av.open_input Sys.argv.(1) in

  let audio, audio_frame =
    try
      let audio_idx, audio_src, _ = Av.find_best_audio_stream src in

      let audio_dst = Avdevice.open_default_audio_output () in
      let _, audio_stream, _ = List.hd (Av.get_audio_streams audio_dst) in

      (Some (audio_idx, audio_dst, audio_stream), [audio_src])
    with Avutil.Error _ -> (None, [])
  in

  let video, video_frame =
    try
      let video_idx, video_src, _ = Av.find_best_video_stream src in

      let video_dst = Avdevice.open_video_output "xv" in
      let _, video_stream, _ = List.hd (Av.get_video_streams video_dst) in

      (Some (video_idx, video_dst, video_stream), [video_src])
    with Avutil.Error _ -> (None, [])
  in

  let rec f () =
    match (Av.read_input ~audio_frame ~video_frame src, audio, video) with
      | `Audio_frame (i, frame), Some (idx, _, stream), _ when i = idx ->
          Av.write_frame stream frame;
          f ()
      | `Video_frame (i, frame), _, Some (idx, _, stream) when i = idx ->
          Av.write_frame stream frame;
          f ()
      | exception Avutil.Error `Eof -> ()
      | _ -> f ()
  in
  f ();

  Av.close src;

  let () = match audio with Some (_, dst, _) -> Av.close dst | None -> () in

  let () = match video with Some (_, dst, _) -> Av.close dst | None -> () in

  Gc.full_major ();
  Gc.full_major ()
