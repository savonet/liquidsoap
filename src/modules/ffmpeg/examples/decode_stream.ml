let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 4 then (
    Printf.eprintf
      "      usage: %s <input file> <output file> <output codec>\n\
      \      API example program to show how to read audio frames from an \
       input file using the streaming API.\n\
      \      This program parse data to packets from a file, decodes them, and \
       writes decoded\n\
      \      audio frames to a audio file named <output file>.\n"
      Sys.argv.(0);
    exit 1);

  Avutil.Log.set_callback (fun _ -> ());

  let in_fd = Unix.openfile Sys.argv.(1) [Unix.O_RDONLY] 0 in
  let out_file = Av.open_output Sys.argv.(2) in
  let codec = Avcodec.Audio.find_encoder_by_name Sys.argv.(3) in
  let channel_layout =
    Avcodec.Audio.find_best_channel_layout codec Avutil.Channel_layout.stereo
  in
  let sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in
  let sample_rate = Avcodec.Audio.find_best_sample_rate codec 44100 in
  let time_base = { Avutil.num = 1; den = sample_rate } in
  let out_stream =
    Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate ~time_base
      ~codec out_file
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
            else Av.get_frame_size out_stream
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
    Av.write_frame out_stream frame
  in

  let write_frame frame =
    let filter = get_filter frame in
    Avfilter.Utils.convert_audio filter on_frame (`Frame frame)
  in

  let read = Unix.read in_fd in
  let seek = Unix.lseek in_fd in
  let container = Av.open_input_stream ~seek read in
  let idx, stream, params = Av.find_best_audio_stream container in
  let codec_id = Avcodec.Audio.get_params_id params in
  let sample_format =
    match Avcodec.Audio.get_sample_format params with
      | `Dbl -> "dbl"
      | `Dblp -> "dblp"
      | `Flt -> "flt"
      | `Fltp -> "fltp"
      | `None -> "none"
      | `S16 -> "s16"
      | `S16p -> "s16p"
      | `S32 -> "s32"
      | `S32p -> "s32p"
      | `S64 -> "s64"
      | `S64p -> "s64p"
      | `U8 -> "u8"
      | `U8p -> "u8p"
  in
  Printf.printf "Detected format: %s, %dHz, %d channels, %s\n%!"
    (Avcodec.Audio.string_of_id codec_id)
    (Avcodec.Audio.get_sample_rate params)
    (Avcodec.Audio.get_nb_channels params)
    sample_format;
  let rec f () =
    try
      (match Av.read_input container ~audio_frame:[stream] with
        | `Audio_frame (i, frame) when i = idx -> write_frame frame
        | _ -> assert false);
      f ()
    with
      | Avutil.Error `Eof ->
          Avfilter.Utils.convert_audio (Option.get !filter) on_frame `Flush
      | Avutil.Error `Invalid_data -> f ()
  in
  f ();

  Unix.close in_fd;
  Av.close container;
  Av.close out_file;

  Gc.full_major ();
  Gc.full_major ()
