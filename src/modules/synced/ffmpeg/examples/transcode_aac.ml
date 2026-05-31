let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf "Usage: %s <input_file> <output_file>.mp4\n" Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Verbose;

  let src = Av.open_input Sys.argv.(1) in

  let idx, istream, params = Av.find_best_audio_stream src in

  let () =
    match Avcodec.descriptor params with
      | Some { name = d; _ } ->
          Printf.printf
            "Input format: %s (channel layout: %s, sample format: %s, sample \
             rate: %d)\n\
             %!"
            d
            (Avutil.Channel_layout.get_description
               (Avcodec.Audio.get_channel_layout params))
            (Option.value ~default:"(unknown)"
               (Avutil.Sample_format.get_name
                  (Avcodec.Audio.get_sample_format params)))
            (Avcodec.Audio.get_sample_rate params)
      | _ -> Printf.printf "Unknown input format..\n!"
  in

  let codec = Avcodec.Audio.find_encoder_by_name "aac" in

  let channel_layout = Avcodec.Audio.get_channel_layout params in
  let sample_format = Avcodec.Audio.get_sample_format params in
  let sample_rate = Avcodec.Audio.get_sample_rate params in
  let time_base = { Avutil.num = 1; den = sample_rate } in

  let ostream =
    Av.open_output Sys.argv.(2)
    |> Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate
         ~time_base ~codec
  in

  let () =
    match Avcodec.descriptor (Av.get_codec_params ostream) with
      | Some { name = d; _ } ->
          Printf.printf
            "Output format: %s (channel layout: %s, sample format: %s, sample \
             rate: %d)\n\
             %!"
            d
            (Avutil.Channel_layout.get_description
               (Avcodec.Audio.get_channel_layout params))
            (Option.value ~default:"(unknown)"
               (Avutil.Sample_format.get_name
                  (Avcodec.Audio.get_sample_format params)))
            (Avcodec.Audio.get_sample_rate params)
      | _ -> Printf.printf "Unknown output format..\n!"
  in

  let filter = ref None in
  let get_filter frame =
    match !filter with
      | Some f -> f
      | None ->
          let in_params =
            {
              Avfilter.Utils.sample_rate =
                Avutil.Audio.frame_get_sample_rate frame;
              channel_layout = Avutil.Audio.frame_get_channel_layout frame;
              sample_format = Avutil.Audio.frame_get_sample_format frame;
            }
          in
          let in_time_base = { Avutil.num = 1; den = sample_rate } in
          let out_frame_size =
            if List.mem `Variable_frame_size (Avcodec.capabilities codec) then
              512
            else Av.get_frame_size ostream
          in
          let out_params =
            { Avfilter.Utils.sample_rate; sample_format; channel_layout }
          in
          let f =
            Avfilter.Utils.init_audio_converter ~in_params ~in_time_base
              ~out_params ~out_frame_size ()
          in
          filter := Some f;
          f
  in

  let pts = ref 0L in
  let on_frame frame =
    Avutil.Frame.set_pts frame (Some !pts);
    pts := Int64.add !pts (Int64.of_int (Avutil.Audio.frame_nb_samples frame));
    Av.write_frame ostream frame
  in

  let write_frame frame =
    let filter = get_filter frame in
    Avfilter.Utils.convert_audio filter on_frame (`Frame frame)
  in

  let rec f () =
    match Av.read_input ~audio_frame:[istream] src with
      | `Audio_frame (i, frame) when i = idx ->
          write_frame frame;
          f ()
      | exception Avutil.Error `Eof -> ()
      | _ -> f ()
  in
  f ();

  Av.get_input istream |> Av.close;
  Av.get_output ostream |> Av.close;

  Gc.full_major ();
  Gc.full_major ()
