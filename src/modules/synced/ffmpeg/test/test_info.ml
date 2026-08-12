open Avutil
open Printf

let test_color_properties () =
  let round_trip what from_name name v =
    Test_assert.checkf
      (from_name (name v) = Some v)
      "%s name/from_name %S" what (name v)
  in
  round_trip "Color_space" Color_space.from_name Color_space.name `Bt709;
  round_trip "Color_range" Color_range.from_name Color_range.name `Mpeg;
  round_trip "Color_primaries" Color_primaries.from_name Color_primaries.name
    `Bt709;
  round_trip "Color_trc" Color_trc.from_name Color_trc.name `Bt709;
  round_trip "Chroma_location" Chroma_location.from_name Chroma_location.name
    `Left;
  printf "\n"

(* The getters must reach the AVClass object rather than the OCaml block
   wrapping it. *)
let test_container_options input =
  let obj = Av.input_obj input in
  let probesize = Options.get_int64 ~name:"probesize" obj in
  Test_assert.checkf (probesize > 0L) "Options.get_int64 probesize = %Ld"
    probesize;
  let max_delay = Options.get_int ~name:"max_delay" obj in
  Test_assert.checkf (max_delay >= -1) "Options.get_int max_delay = %d"
    max_delay

let test_file_info url =
  let input = Av.open_input url in
  test_container_options input;
  Test_assert.checkf
    (Av.get_audio_streams input <> []
    || Av.get_video_streams input <> []
    || Av.get_subtitle_streams input <> [])
    "%s has at least one stream" url;
  printf "%s (%s s) :\n" url
    (match Av.get_input_duration input with
      | None -> "N/A"
      | Some d -> Int64.to_string d);
  Av.get_input_metadata input
  |> List.iter (fun (k, v) -> printf "\t%s : %s\n" k v);
  Av.get_audio_streams input
  |> List.iter (fun (idx, stm, cd) ->
      Av.get_metadata stm |> List.iter (fun (k, v) -> printf "\t%s : %s\n" k v);
      let tb = Av.get_container_stream_time_base ~index:idx input in
      printf "\tAudio stream %d container time_base: %d/%d\n" idx tb.num tb.den;
      Avcodec.Audio.(
        Test_assert.checkf
          (get_sample_rate cd > 0 && get_nb_channels cd > 0)
          "audio stream %d has a sample rate and channels" idx;
        printf "\tAudio stream %d : %s %s, %s %s, %s %d, %s %d, %s %d, %s %s\n"
          idx "codec"
          (get_params_id cd |> string_of_id)
          "sample format"
          ( get_sample_format cd |> fun p ->
            Option.get (Sample_format.get_name p) )
          "channels" (get_nb_channels cd) "bit rate" (get_bit_rate cd)
          "sample rate" (get_sample_rate cd) "duration (ms)"
          (match Av.get_duration ~format:`Millisecond stm with
            | None -> "N/A"
            | Some v -> Int64.to_string v)));
  Av.get_video_streams input
  |> List.iter (fun (idx, stm, cd) ->
      Av.get_metadata stm |> List.iter (fun (k, v) -> printf "\t%s : %s\n" k v);
      let tb = Av.get_container_stream_time_base ~index:idx input in
      printf "\tVideo stream %d container time_base: %d/%d\n" idx tb.num tb.den;
      Avcodec.Video.(
        let sar = get_sample_aspect_ratio cd in
        Test_assert.checkf
          (get_width cd > 0 && get_height cd > 0)
          "video stream %d has a width and height" idx;
        printf
          "\tVideo stream %d : %s %s, %s %d, %s %d, %s %d / %d, %s %d, %s %s\n"
          idx "codec"
          (get_params_id cd |> string_of_id)
          "width" (get_width cd) "height" (get_height cd) "sample aspect ratio"
          sar.num sar.den "bit rate" (get_bit_rate cd) "duration (ns)"
          (match Av.get_duration ~format:`Millisecond stm with
            | None -> "N/A"
            | Some v -> Int64.to_string v);
        let decoder =
          create_decoder ~params:cd (find_decoder (get_params_id cd))
        in
        let got_frame = ref false in
        let rec read_frame () =
          match Av.read_input ~video_frame:[stm] input with
            | `Video_frame (_, frame) ->
                got_frame := true;
                let cs = Video.frame_get_color_space frame in
                let cr = Video.frame_get_color_range frame in
                let cp = Video.frame_get_color_primaries frame in
                let ct = Video.frame_get_color_trc frame in
                let cl = Video.frame_get_chroma_location frame in
                printf
                  "\t\tFirst frame color_space: %s, color_range: %s, \
                   color_primaries: %s, color_trc: %s, chroma_location: %s\n"
                  (Color_space.name cs) (Color_range.name cr)
                  (Color_primaries.name cp) (Color_trc.name ct)
                  (Chroma_location.name cl)
            | exception Avutil.Error `Eof -> ()
            | _ -> read_frame ()
        in
        read_frame ();
        Test_assert.checkf !got_frame "decoded a frame from video stream %d" idx;
        ignore decoder));
  Av.get_subtitle_streams input
  |> List.iter (fun (idx, stm, cd) ->
      Av.get_metadata stm |> List.iter (fun (k, v) -> printf "\t%s : %s\n" k v);
      let tb = Av.get_container_stream_time_base ~index:idx input in
      printf "\tSubtitle stream %d container time_base: %d/%d\n" idx tb.num
        tb.den;
      Avcodec.Subtitle.(
        printf "\tSubtitle stream %d : %s %s, %s %s\n" idx "codec"
          (get_params_id cd |> string_of_id)
          "duration (us)"
          (match Av.get_duration ~format:`Millisecond stm with
            | None -> "N/A"
            | Some v -> Int64.to_string v)));
  printf "\n"

let () =
  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  test_color_properties ();

  let urls = Sys.argv |> Array.to_list |> List.tl in
  Test_assert.check "at least one input file was given" (urls <> []);
  List.iter test_file_info urls;
  Test_assert.finish ()
