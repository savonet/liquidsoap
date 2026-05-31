let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf "usage: %s input_file output_file\n" Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Info;
  Avutil.Log.set_callback print_string;

  let decoder = Avcodec.Video.find_decoder_by_name "libvpx-vp9" in
  let encoder = Avcodec.Video.find_encoder_by_name "libvpx-vp9" in

  let configure_video_stream _params =
    { Av.codec = Some decoder; opts = None }
  in

  let src = Av.open_input ~configure_video_stream Sys.argv.(1) in

  let ivss = Av.get_video_streams src in

  let dst = Av.open_output Sys.argv.(2) in

  let ovss =
    List.map
      (fun (i, _, params) ->
        let width = Avcodec.Video.get_width params in
        let height = Avcodec.Video.get_height params in
        let pixel_format =
          match Avcodec.Video.get_pixel_format params with
            | None -> failwith "Pixel format unknown!"
            | Some f -> f
        in
        Printf.printf "Stream %d: %dx%d pixel_format=%s\n%!" i width height
          (match Avutil.Pixel_format.to_string pixel_format with
            | Some s -> s
            | None -> "unknown");
        let time_base =
          Av.get_time_base
            (let _, s, _ = List.nth ivss 0 in
             s)
        in
        ( i,
          Av.new_video_stream ~pixel_format ~width ~height ~time_base
            ~codec:encoder dst ))
      ivss
  in

  let rec loop () =
    match
      Av.read_input ~video_frame:(List.map (fun (_, s, _) -> s) ivss) src
    with
      | `Video_frame (i, frame) ->
          Av.write_frame (List.assoc i ovss) frame;
          loop ()
      | exception Avutil.Error `Eof -> ()
      | _ -> assert false
  in
  loop ();

  Av.close src;
  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
