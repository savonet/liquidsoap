(* Test on_unhandled_packet callback in Av.read_input.
   Uses a file with audio, video and subtitle streams.
   We request only audio frames and check that video and subtitle
   packets are delivered via the on_unhandled_packet callback. *)

let () =
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Usage: %s input_file\n" Sys.argv.(0);
    exit 1);

  let input_file = Sys.argv.(1) in
  Printf.printf "Opening: %s\n%!" input_file;

  let src = Av.open_input input_file in

  let audio_streams = Av.get_audio_streams src in
  let video_streams = Av.get_video_streams src in
  let subtitle_streams = Av.get_subtitle_streams src in

  Printf.printf "Streams: %d audio, %d video, %d subtitle\n%!"
    (List.length audio_streams)
    (List.length video_streams)
    (List.length subtitle_streams);

  let unhandled_audio = ref 0 in
  let unhandled_video = ref 0 in
  let unhandled_subtitle = ref 0 in
  let unhandled_data = ref 0 in
  let handled_audio = ref 0 in

  let on_unhandled_packet = function
    | `Audio_packet _ -> incr unhandled_audio
    | `Video_packet _ -> incr unhandled_video
    | `Subtitle_packet _ -> incr unhandled_subtitle
    | `Data_packet _ -> incr unhandled_data
  in

  (* Only request audio frames; video/subtitle packets should be unhandled. *)
  let rec read_loop () =
    match
      Av.read_input ~on_unhandled_packet
        ~audio_frame:(List.map (fun (_, s, _) -> s) audio_streams)
        src
    with
      | `Audio_frame _ ->
          incr handled_audio;
          read_loop ()
      | exception Avutil.Error `Eof -> ()
      | exception Avutil.Error err ->
          Printf.eprintf "Error: %s\n%!" (Avutil.string_of_error err);
          exit 1
      | _ -> read_loop ()
  in
  read_loop ();

  Av.close src;

  Printf.printf "Handled audio frames: %d\n%!" !handled_audio;
  Printf.printf "Unhandled audio packets: %d\n%!" !unhandled_audio;
  Printf.printf "Unhandled video packets: %d\n%!" !unhandled_video;
  Printf.printf "Unhandled subtitle packets: %d\n%!" !unhandled_subtitle;
  Printf.printf "Unhandled data packets: %d\n%!" !unhandled_data;

  if !handled_audio = 0 then (
    Printf.eprintf "FAIL: expected some handled audio frames\n%!";
    exit 1);

  if !unhandled_video = 0 && List.length video_streams > 0 then (
    Printf.eprintf "FAIL: expected some unhandled video packets\n%!";
    exit 1);

  Printf.printf "PASS: on_unhandled_packet works correctly\n%!";
  Gc.full_major ()
