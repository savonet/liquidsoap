let () =
  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let format = Option.get (Av.Format.find_input_format "v4l2") in
  let src = Av.open_input ~format "/dev/video0" in

  let () =
    print_endline "Available outputs:";
    List.iter
      (fun f -> print_endline ("- " ^ Av.Format.get_output_name f))
      (Avdevice.get_video_output_formats ())
  in

  let video, video_frame =
    try
      let video_idx, video_src, _ = Av.find_best_video_stream src in
      let video_dst = Avdevice.open_video_output "sdl2" in
      let _, video_stream, _ = List.hd (Av.get_video_streams video_dst) in
      (Some (video_idx, video_dst, video_stream), [video_src])
    with Avutil.Error _ -> (None, [])
  in

  let rec f () =
    match (Av.read_input ~video_frame src, video) with
      | `Video_frame (i, frame), Some (idx, _, stream) when i = idx ->
          Av.write_frame stream frame;
          f ()
      | exception Avutil.Error `Eof -> ()
      | _ -> f ()
  in
  f ();
  Av.close src;
  let () = match video with Some (_, dst, _) -> Av.close dst | None -> () in
  Gc.full_major ();
  Gc.full_major ()
