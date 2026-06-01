open Avutil
module Converter = Swscale.Make (Swscale.PackedBigArray) (Swscale.Frame)

let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv < 3 then (
    Printf.eprintf
      "Usage: %s <input_file> <output.jpg>\n\n\
       Extract the first bitmap subtitle from a media file and save it as JPEG.\n"
      Sys.argv.(0);
    exit 1)

let () =
  Log.set_level `Warning;
  Log.set_callback print_string;

  let input_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in

  let src = Av.open_input input_file in

  let _, subtitle_stream, _ = Av.find_best_subtitle_stream src in

  (* Read frames until we find a bitmap subtitle with picture data *)
  let rec find_bitmap () =
    match Av.read_input ~subtitle_frame:[subtitle_stream] src with
      | `Subtitle_frame (_, frame) -> (
          let content = Subtitle.get_content frame in
          let bitmap_rect =
            List.find_opt
              (fun (rect : Subtitle.rectangle) ->
                rect.rect_type = `Bitmap && rect.pict <> None)
              content.rectangles
          in
          match bitmap_rect with Some rect -> rect | None -> find_bitmap ())
      | exception Error `Eof ->
          Printf.eprintf "No bitmap subtitle found in %s\n" input_file;
          Av.close src;
          exit 1
      | _ -> find_bitmap ()
  in
  let rect = find_bitmap () in
  Av.close src;

  let pict = Option.get rect.pict in
  Printf.printf "Found bitmap subtitle: %dx%d at (%d,%d), %d colors\n" pict.w
    pict.h pict.x pict.y pict.nb_colors;

  (* Convert from Pal8 to Yuvj420p using swscale *)
  let sws = Converter.create [] pict.w pict.h `Pal8 pict.w pict.h `Yuvj420p in
  let out_frame = Converter.convert sws pict.planes in

  (* Encode as JPEG *)
  let dst = Av.open_output output_file in
  let codec = Avcodec.Video.find_encoder_by_name "mjpeg" in
  let frame_rate = { Avutil.num = 1; den = 1 } in
  let time_base = { Avutil.num = 1; den = 1 } in
  let ovs =
    Av.new_video_stream ~width:pict.w ~height:pict.h ~frame_rate ~time_base
      ~pixel_format:`Yuvj420p ~codec dst
  in
  Frame.set_pts out_frame (Some 0L);
  Av.write_frame ovs out_frame;
  Av.close dst;

  Printf.printf "Saved bitmap subtitle to %s\n" output_file;

  Gc.full_major ();
  Gc.full_major ()
