open Avutil

let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf
      "Usage: %s input_file output_file [codec_name]\n\n\
       Remux subtitles by explicitly extracting and recreating frame content.\n\n\
       Arguments:\n\
      \  input_file   - Input file containing subtitles\n\
      \  output_file  - Output file (format determined by extension)\n\
      \  codec_name   - Optional: encoder name (e.g., subrip, webvtt, ass)\n"
      Sys.argv.(0);
    exit 1)

let string_of_subtitle_type = function
  | `None -> "none"
  | `Bitmap -> "bitmap"
  | `Text -> "text"
  | `Ass -> "ass"

let () =
  Log.set_level `Warning;
  Log.set_callback print_string;

  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  let src = Av.open_input input_file in
  let dst = Av.open_output output_file in

  let input_subtitle_streams = Av.get_subtitle_streams src in

  if List.length input_subtitle_streams = 0 then (
    Printf.eprintf "No subtitle streams found in %s\n" input_file;
    Av.close src;
    exit 1);

  Printf.printf "Found %d subtitle stream(s) in input\n"
    (List.length input_subtitle_streams);

  let time_base = time_base () in
  let output_streams =
    input_subtitle_streams
    |> List.map (fun (i, _stream, params) ->
        let input_codec_id = Avcodec.Subtitle.get_params_id params in
        Printf.printf "  Stream %d: %s\n" i
          (Avcodec.Subtitle.string_of_id input_codec_id);

        let codec =
          if Array.length Sys.argv > 3 then
            Avcodec.Subtitle.find_encoder_by_name Sys.argv.(3)
          else (
            try Avcodec.Subtitle.find_encoder input_codec_id
            with _ -> Avcodec.Subtitle.find_encoder_by_name "subrip")
        in
        Printf.printf "    -> Encoding to: %s\n"
          (Avcodec.Subtitle.get_name codec);

        (i, Av.new_subtitle_stream ~time_base ~codec dst))
  in

  Printf.printf "\nRemuxing subtitles using explicit content extraction...\n%!";

  let subtitle_count = ref 0 in

  let rec process () =
    match
      Av.read_input
        ~subtitle_frame:(List.map (fun (_, s, _) -> s) input_subtitle_streams)
        src
    with
      | `Subtitle_frame (i, frame) ->
          incr subtitle_count;

          (* Extract full content from decoded frame *)
          let content = Subtitle.get_content frame in

          Printf.printf "  [%d] pts=%s format=%d display=%d-%dms rects=%d\n"
            !subtitle_count
            (match content.pts with
              | Some p -> Int64.to_string p
              | None -> "N/A")
            content.format content.start_display_time content.end_display_time
            (List.length content.rectangles);
          List.iter
            (fun (rect : Subtitle.rectangle) ->
              Printf.printf "       type=%s text=%S ass=%S\n"
                (string_of_subtitle_type rect.rect_type)
                rect.text rect.ass)
            content.rectangles;

          let content =
            {
              Subtitle.format = 1;
              start_display_time = content.start_display_time;
              end_display_time = content.end_display_time;
              rectangles =
                List.map
                  (fun { Subtitle.pict; flags; rect_type; text; ass } ->
                    { Subtitle.pict; flags; rect_type; text; ass })
                  content.rectangles;
              pts = content.pts;
            }
          in

          (* Create new frame from the extracted content *)
          let new_frame = Subtitle.create_frame content in

          (try Av.write_subtitle_frame (List.assoc i output_streams) new_frame
           with Error err ->
             Printf.eprintf "Warning: Failed to write subtitle %d: %s\n"
               !subtitle_count (string_of_error err));
          process ()
      | exception Error `Eof -> ()
      | exception Error err ->
          Printf.eprintf "Error reading input: %s\n" (string_of_error err)
      | _ -> process ()
  in
  process ();

  Printf.printf "\nRemuxed %d subtitle(s)\n" !subtitle_count;

  Av.close src;
  Av.close dst;

  Printf.printf "Output written to: %s\n" output_file;

  Gc.full_major ();
  Gc.full_major ()
