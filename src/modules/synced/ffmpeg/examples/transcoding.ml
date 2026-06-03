let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf "usage: %s input_file output_file\n" Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let src = Av.open_input Sys.argv.(1) in
  let dst = Av.open_output Sys.argv.(2) in
  let audio_codec = Avcodec.Audio.find_encoder_by_name "aac" in
  let video_codec = Avcodec.Video.find_encoder_by_name "mpeg4" in

  let iass = Av.get_audio_streams src in

  let oass =
    iass
    |> List.map (fun (i, _, params) ->
        let channel_layout = Avcodec.Audio.get_channel_layout params in
        let sample_format = Avcodec.Audio.get_sample_format params in
        let sample_rate = Avcodec.Audio.get_sample_rate params in
        let time_base = { Avutil.num = 1; den = sample_rate } in
        ( i,
          Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate
            ~time_base ~codec:audio_codec dst ))
  in

  let frame_rate = { Avutil.num = 25; den = 1 } in
  let time_base = { Avutil.num = 1; den = 25 } in

  let ivss = Av.get_video_streams src in

  let ovss =
    ivss
    |> List.map (fun (i, _, params) ->
        let width = Avcodec.Video.get_width params in
        let height = Avcodec.Video.get_height params in
        let pixel_format =
          match Avcodec.Video.get_pixel_format params with
            | None -> failwith "Pixel format unknown!"
            | Some f -> f
        in
        ( i,
          Av.new_video_stream ~pixel_format ~frame_rate ~time_base ~width
            ~height ~codec:video_codec dst ))
  in

  let isss = Av.get_subtitle_streams src in

  let osss =
    isss
    |> List.map (fun (i, _, params) ->
        let codec =
          Avcodec.Subtitle.find_encoder (Avcodec.Subtitle.get_params_id params)
        in
        (i, Av.new_subtitle_stream ~time_base ~codec dst))
  in

  let process_frame frame =
    let metadata = Avutil.Frame.metadata frame in
    List.iter
      (fun (key, value) -> Printf.printf "Frame metadata: %s->%s\n%!" key value)
      metadata;
    Avutil.Frame.set_metadata frame (("encoder", "ocaml-ffmpeg") :: metadata)
  in

  let rec f () =
    match
      Av.read_input
        ~audio_frame:(List.map (fun (_, s, _) -> s) iass)
        ~video_frame:(List.map (fun (_, s, _) -> s) ivss)
        ~subtitle_frame:(List.map (fun (_, s, _) -> s) isss)
        src
    with
      | `Audio_frame (i, frame) ->
          process_frame frame;
          Av.write_frame (List.assoc i oass) frame;
          f ()
      | `Video_frame (i, frame) ->
          process_frame frame;
          Av.write_frame (List.assoc i ovss) frame;
          f ()
      | `Subtitle_frame (i, frame) ->
          Av.write_subtitle_frame (List.assoc i osss) frame;
          f ()
      | exception Avutil.Error `Eof -> ()
      | _ -> assert false
  in
  f ();

  Av.close src;
  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
