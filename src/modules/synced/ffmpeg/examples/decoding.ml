let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "usage: %s input_file\n" Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let src = Av.open_input Sys.argv.(1) in

  let mk_audio_decoder (pos, _, params) =
    ( pos,
      Avcodec.Audio.create_decoder ~params
        Avcodec.Audio.(find_decoder (get_params_id params)) )
  in

  let iass = Av.get_audio_streams src in

  let dass = List.map mk_audio_decoder iass in

  let mk_video_decoder (pos, _, params) =
    ( pos,
      Avcodec.Video.create_decoder ~params
        Avcodec.Video.(find_decoder (get_params_id params)) )
  in

  let ivss = Av.get_video_streams src in

  let dvss = List.map mk_video_decoder ivss in

  let rec f () =
    match
      Av.read_input
        ~audio_packet:(List.map (fun (_, s, _) -> s) iass)
        ~video_packet:(List.map (fun (_, s, _) -> s) ivss)
        src
    with
      | `Audio_packet (i, pkt) ->
          Avcodec.decode (List.assoc i dass) (fun _ -> ()) pkt;
          f ()
      | `Video_packet (i, pkt) ->
          Avcodec.decode (List.assoc i dvss) (fun _ -> ()) pkt;
          f ()
      | exception Avutil.Error `Eof -> ()
      | _ -> assert false
  in
  f ();

  List.iter (fun (_, d) -> Avcodec.flush_decoder d (fun _ -> ())) dass;
  List.iter (fun (_, d) -> Avcodec.flush_decoder d (fun _ -> ())) dvss;

  Av.close src;

  Gc.full_major ();
  Gc.full_major ()
