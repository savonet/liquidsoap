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

  let audio_input, oass =
    Av.find_best_audio_stream src |> fun (i, audio_input, params) ->
    let channel_layout = Avcodec.Audio.get_channel_layout params in
    let sample_format = Avcodec.Audio.get_sample_format params in
    let sample_rate = Avcodec.Audio.get_sample_rate params in
    let time_base = { Avutil.num = 1; den = sample_rate } in
    ( audio_input,
      ( i,
        Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate
          ~time_base ~codec:audio_codec dst ) )
  in

  let frame_rate = { Avutil.num = 25; den = 1 } in
  let time_base = { Avutil.num = 1; den = 25 } in

  let video_params, video_input, ovss =
    Av.find_best_video_stream src |> fun (i, video_input, params) ->
    let width = Avcodec.Video.get_width params in
    let height = Avcodec.Video.get_height params in
    let pixel_format =
      match Avcodec.Video.get_pixel_format params with
        | None -> failwith "Pixel format unknown!"
        | Some f -> f
    in
    ( params,
      video_input,
      ( i,
        Av.new_video_stream ~pixel_format ~frame_rate ~time_base ~width ~height
          ~codec:video_codec dst ) )
  in

  let filter =
    let config = Avfilter.init () in
    let _buffer =
      let time_base = Av.get_time_base video_input in
      let pixel_aspect = Av.get_pixel_aspect video_input in
      let pixel_format =
        match Avcodec.Video.get_pixel_format video_params with
          | None -> failwith "Pixel format unknown!"
          | Some f -> f
      in
      let width = Avcodec.Video.get_width video_params in
      let height = Avcodec.Video.get_height video_params in
      let args =
        [
          `Pair ("video_size", `String (Printf.sprintf "%dx%x" width height));
          `Pair ("pix_fmt", `Int (Avutil.Pixel_format.get_id pixel_format));
          `Pair ("time_base", `Rational time_base);
        ]
        @
          match pixel_aspect with
          | Some p -> [`Pair ("pixel_aspect", `Rational p)]
          | None -> []
      in
      Avfilter.(attach ~args ~name:"buffer" buffer config)
    in
    let fps =
      let args =
        [`Pair ("fps", `String (Avutil.string_of_rational frame_rate))]
      in
      let fps = Avfilter.find "fps" in
      Avfilter.attach ~args ~name:"fps" fps config
    in
    let sink = Avfilter.(attach ~name:"sink" buffersink config) in
    Avfilter.link
      (List.hd Avfilter.(_buffer.io.outputs.video))
      (List.hd Avfilter.(fps.io.inputs.video));
    Avfilter.link
      (List.hd Avfilter.(fps.io.outputs.video))
      (List.hd Avfilter.(sink.io.inputs.video));
    Avfilter.launch config
  in

  let _, output = List.hd Avfilter.(filter.outputs.video) in
  let context = output.context in
  let time_base = Avfilter.time_base context in
  let frame_rate = Avfilter.frame_rate context in
  let pixel_aspect = Avfilter.pixel_aspect context in
  Printf.printf
    "Sink info:\n\
     time_base: %d/%d\n\
     frame_rate: %d/%d\n\
     width: %d\n\
     height: %d\n\
     pixel_aspect: %s\n"
    time_base.Avutil.num time_base.Avutil.den frame_rate.Avutil.num
    frame_rate.Avutil.den (Avfilter.width context) (Avfilter.height context)
    (match pixel_aspect with
      | None -> "0/1"
      | Some { Avutil.num; den } -> Printf.sprintf "%d/%d" num den);

  let process_video i frm =
    try
      let stream = List.assoc i [ovss] in
      let _, input = List.hd Avfilter.(filter.inputs.video) in
      input frm;
      let rec flush () =
        try
          Av.write_frame stream (output.handler ());
          flush ()
        with Avutil.Error `Eagain -> ()
      in
      flush ()
    with
      | Not_found -> ()
      | Avutil.Error `Eof when frm = `Flush -> ()
  in

  let process_audio i frm =
    try
      let stream = List.assoc i [oass] in
      Av.write_frame stream frm
    with Not_found -> ()
  in

  let rec f () =
    match
      Av.read_input ~audio_frame:[audio_input] ~video_frame:[video_input] src
    with
      | `Audio_frame (i, frame) ->
          process_audio i frame;
          f ()
      | `Video_frame (i, frame) ->
          process_video i (`Frame frame);
          f ()
      | exception Avutil.Error `Eof -> process_video (fst ovss) `Flush
      | _ -> f ()
  in
  f ();

  Av.close src;
  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
