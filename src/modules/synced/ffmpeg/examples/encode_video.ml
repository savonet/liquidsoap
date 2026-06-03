open Avutil

let () = Printexc.record_backtrace true

let fill_yuv_image width height frame_index planes =
  (* Y *)
  let data_y, linesize_y = planes.(0) in

  for y = 0 to height - 1 do
    let off = y * linesize_y in
    for x = 0 to width - 1 do
      data_y.{x + off} <- x + y + (frame_index * 3)
    done
  done;

  (* Cb and Cr *)
  let data_cb, linesize_cb = planes.(1) in
  let data_cr, _ = planes.(2) in

  for y = 0 to (height / 2) - 1 do
    let off = y * linesize_cb in
    for x = 0 to (width / 2) - 1 do
      data_cb.{x + off} <- 128 + y + (frame_index * 2);
      data_cr.{x + off} <- 64 + x + (frame_index * 5)
    done
  done

let () =
  if Array.length Sys.argv < 4 then (
    Printf.eprintf "Usage: %s <output file> <pixel format> <codec name>\n"
      Sys.argv.(0);
    exit 1);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let width = 352 in
  let height = 288 in
  let pixel_format = Avutil.Pixel_format.of_string Sys.argv.(2) in

  let () =
    let string_of_flag = function
      | `Be -> "be"
      | `Pal -> "pal"
      | `Bitstream -> "bitstream"
      | `Hwaccel -> "hwaccel"
      | `Planar -> "planar"
      | `Rgb -> "rgb"
      | `Pseudopal -> "pseudopal"
      | `Alpha -> "alpha"
      | `Bayer -> "bayer"
      | `Float -> "float"
      | `Xyz -> "xyz"
    in
    let string_of_comp { Avutil.Pixel_format.plane; step; shift; offset; depth }
        =
      Printf.sprintf "plane: %i, step: %i, shift: %i, offset: %i, depth: %i"
        plane step shift offset depth
    in
    let descriptor = Avutil.Pixel_format.descriptor pixel_format in
    Printf.printf
      "Pixel format:\n\
       name: %s\n\
       nb_components: %i\n\
       log2_chroma_w: %i\n\
       log2_chroma_h: %i\n\
       flags: %s\n\
       comp: [\n\
      \  %s\n\
       ]\n\
       alias: %s\n\
       bits: %i\n"
      descriptor.Avutil.Pixel_format.name
      descriptor.Avutil.Pixel_format.nb_components
      descriptor.Avutil.Pixel_format.log2_chroma_w
      descriptor.Avutil.Pixel_format.log2_chroma_h
      (String.concat ", "
         (List.map string_of_flag descriptor.Avutil.Pixel_format.flags))
      (String.concat ",\n  "
         (List.map string_of_comp descriptor.Avutil.Pixel_format.comp))
      (match descriptor.Avutil.Pixel_format.alias with
        | None -> "N/A"
        | Some a -> a)
      (Avutil.Pixel_format.bits descriptor)
  in

  let codec = Avcodec.Video.find_encoder_by_name Sys.argv.(3) in

  let dst = Av.open_output Sys.argv.(1) in

  let frame_rate = { Avutil.num = 25; den = 1 } in
  let time_base = { Avutil.num = 1; den = 25 } in
  let pts = ref 0L in

  let ovs =
    Av.new_video_stream ~width ~height ~frame_rate ~time_base ~pixel_format
      ~codec dst
  in

  let frame = Video.create_frame width height pixel_format in

  for i = 0 to 240 do
    Video.frame_visit ~make_writable:true (fill_yuv_image width height i) frame
    |> fun frame ->
    Avutil.Frame.set_pts frame (Some !pts);
    pts := Int64.succ !pts;
    Av.write_frame ovs frame
  done;

  Av.close dst;

  Gc.full_major ();
  Gc.full_major ()
