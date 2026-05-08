let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 2 then (
    Printf.(
      Av.Format.(
        printf "\ninput devices :\n";
        Avdevice.get_audio_input_formats ()
        |> List.iter (fun d ->
            printf "\t%s (%s)\n" (get_input_name d) (get_input_long_name d));
        printf "\noutput devices :\n";
        Avdevice.get_audio_output_formats ()
        |> List.iter (fun d ->
            printf "\t%s (%s)\n" (get_output_name d) (get_output_long_name d));
        printf
          "\n\
           usage: %s input [output]\n\
           input and output can be devices or file names\n"
          Sys.argv.(0);
        exit 0)));

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let src =
    try Avdevice.open_audio_input Sys.argv.(1)
    with Avutil.Error _ -> Av.open_input Sys.argv.(1)
  in

  let idx, ias, params = Av.find_best_audio_stream src in

  let codec = Avcodec.Audio.find_encoder_by_name "flac" in
  let channel_layout = Avcodec.Audio.get_channel_layout params in
  let sample_format = Avcodec.Audio.get_sample_format params in
  let sample_rate = Avcodec.Audio.get_sample_rate params in
  let time_base = { Avutil.num = 1; den = sample_rate } in

  let dst =
    try
      if Array.length Sys.argv < 3 then Avdevice.open_default_audio_output ()
      else Avdevice.open_audio_output Sys.argv.(2)
    with Avutil.Error _ -> Av.open_output Sys.argv.(2)
  in
  let dst_stream =
    Av.new_audio_stream ~channel_layout ~sample_format ~sample_rate ~time_base
      ~codec dst
  in

  Avdevice.Dev_to_app.(
    set_control_message_callback (function
      | Volume_level_changed v ->
          Printf.printf "Volume level changed to %f %%\n" (v *. 100.)
      | _ -> print_endline "Unexpected dev to app control message"))
    dst;

  (try Avdevice.App_to_dev.(control_messages [Get_volume; Set_volume 0.3]) dst
   with Avutil.Error err -> prerr_endline (Avutil.string_of_error err));

  let rec run n =
    if n > 0 then (
      try
        (match Av.read_input src ~audio_frame:[ias] with
          | `Audio_frame (i, frame) when i = idx ->
              Av.write_frame dst_stream frame
          | _ -> assert false);
        run (n - 1)
      with Avutil.Error `Eof -> ())
  in
  run 500;

  Av.close src;
  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
