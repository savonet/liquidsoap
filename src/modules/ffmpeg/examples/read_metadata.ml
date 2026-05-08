let () = Printexc.record_backtrace true

let () =
  assert (Array.length Sys.argv >= 2);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let file = Sys.argv.(1) in
  let f = Av.open_input file in
  let md = Av.get_input_metadata f in
  List.iter (fun (l, v) -> Printf.printf "Format metadata: %s: %s\n" l v) md;
  List.iter
    (fun (i, s, _) ->
      List.iter
        (fun (l, v) -> Printf.printf "Audio stream %d metadata: %s: %s\n" i l v)
        (Av.get_metadata s))
    (Av.get_audio_streams f);
  List.iter
    (fun (i, s, _) ->
      List.iter
        (fun (l, v) -> Printf.printf "Video stream %d metadata: %s: %s\n" i l v)
        (Av.get_metadata s))
    (Av.get_video_streams f);
  Av.close f;

  Gc.full_major ()
