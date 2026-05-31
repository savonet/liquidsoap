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

  let audio_params, audio_input, idx =
    Av.find_best_audio_stream src |> fun (i, audio_input, params) ->
    (params, audio_input, i)
  in

  let filter =
    let config = Avfilter.init () in
    let time_base = Av.get_time_base audio_input in
    let sample_rate = Avcodec.Audio.get_sample_rate audio_params in
    let sample_format =
      Avutil.Sample_format.get_id (Avcodec.Audio.get_sample_format audio_params)
    in
    let channel_layout =
      Avutil.Channel_layout.get_description
        (Avcodec.Audio.get_channel_layout audio_params)
    in
    let abuffer_node =
      let args =
        [
          `Pair ("time_base", `Rational time_base);
          `Pair ("sample_rate", `Int sample_rate);
          `Pair ("sample_fmt", `Int sample_format);
          `Pair ("channel_layout", `String channel_layout);
        ]
      in
      Avfilter.attach ~args ~name:"in" Avfilter.abuffer config
    in
    let aresample_filter = Avfilter.find "aresample" in
    let aresample =
      let args = [`Pair ("sample_rate", `Int 22050)] in
      Avfilter.attach ~args ~name:"aresample" aresample_filter config
    in
    let aformat_filter = Avfilter.find "aformat" in
    let aformat =
      let aformat_arg name values =
        match
          Avfilter.get_array_separator ~filter_name:"aformat" ~option_name:name
        with
          | _ -> `Pair (name, `Array (List.map (fun s -> `String s) values))
          | exception _ -> `Pair (name, `String (String.concat "|" values))
      in
      let args =
        [
          aformat_arg "sample_fmts" ["s16"; "fltp"];
          aformat_arg "channel_layouts" ["stereo"; "mono"];
        ]
      in
      Avfilter.attach ~args ~name:"aformat" aformat_filter config
    in
    let sink_node = Avfilter.(attach ~name:"out" abuffersink config) in
    Avfilter.link
      (List.hd Avfilter.(abuffer_node.io.outputs.audio))
      (List.hd Avfilter.(aresample.io.inputs.audio));
    Avfilter.link
      (List.hd Avfilter.(aresample.io.outputs.audio))
      (List.hd Avfilter.(aformat.io.inputs.audio));
    Avfilter.link
      (List.hd Avfilter.(aformat.io.outputs.audio))
      (List.hd Avfilter.(sink_node.io.inputs.audio));
    Avfilter.launch config
  in

  let _, output = List.hd Avfilter.(filter.outputs.audio) in
  let context = output.context in
  let time_base = Avfilter.time_base context in
  let filter_sample_format = Avfilter.sample_format context in
  let filter_channel_layout = Avfilter.channel_layout context in
  let filter_sample_rate = Avfilter.sample_rate context in

  Printf.printf
    "Sink info:\n\
     time_base: %d/%d\n\
     channels: %d\n\
     channel_layout: %s\n\
     sample_rate: %d\n\
     sample_format: %s\n"
    time_base.Avutil.num time_base.Avutil.den
    (Avfilter.channels context)
    (Avutil.Channel_layout.get_description filter_channel_layout)
    filter_sample_rate
    (match Avutil.Sample_format.get_name filter_sample_format with
      | None -> "none"
      | Some n -> n);

  let oass =
    Av.new_audio_stream ~channel_layout:filter_channel_layout
      ~sample_format:filter_sample_format ~sample_rate:filter_sample_rate
      ~time_base ~codec:audio_codec dst
  in

  let frame_size =
    if List.mem `Variable_frame_size (Avcodec.capabilities audio_codec) then 512
    else Av.get_frame_size oass
  in

  Avfilter.set_frame_size context frame_size;

  let process_audio i frm =
    try
      assert (i = idx);
      let _, input = List.hd Avfilter.(filter.inputs.audio) in
      input frm;
      let rec flush () =
        try
          Av.write_frame oass (output.handler ());
          flush ()
        with Avutil.Error `Eagain -> ()
      in
      flush ()
    with Not_found -> ()
  in

  Gc.full_major ();
  Gc.full_major ();

  let rec f () =
    match Av.read_input ~audio_frame:[audio_input] src with
      | `Audio_frame (i, frame) ->
          process_audio i (`Frame frame);
          f ()
      | exception Avutil.Error `Eof -> (
          try process_audio idx `Flush with Avutil.Error `Eof -> ())
      | _ -> f ()
  in
  f ();

  Av.close src;
  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
