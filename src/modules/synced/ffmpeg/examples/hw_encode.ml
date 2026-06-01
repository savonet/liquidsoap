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
    Printf.eprintf "Usage: %s <output file> <codec name> <mode>\n" Sys.argv.(0);
    exit 1);
  let dst = Av.open_output Sys.argv.(1) in
  let codec_name = Sys.argv.(2) in
  let mode = Sys.argv.(3) in
  let codec () =
    try Avcodec.Video.find_encoder_by_name codec_name
    with _ ->
      Printf.printf "encoder %s not available!\n" codec_name;
      exit 0
  in
  let codec_name = Avcodec.name (codec ()) in
  let configs = Avcodec.hw_configs (codec ()) in
  Printf.printf "hw_config for %s:\n%s\n" codec_name
    (String.concat ",\n"
       (List.map
          (fun { Avcodec.pixel_format; methods; device_type } ->
            Printf.sprintf
              "{\n  pixel_format: %s,\n  methods: %s,\n  device_type: %s\n}"
              (match Avutil.Pixel_format.to_string pixel_format with
                | None -> "none"
                | Some f -> f)
              (String.concat ", "
                 (List.map
                    (function
                      | `Hw_device_ctx -> "hw_device_ctx"
                      | `Hw_frames_ctx -> "hw_frames_ctx"
                      | `Internal -> "internal"
                      | `Ad_hoc -> "ad-hoc")
                    methods))
              (match device_type with
                | `None -> "none"
                | `Vdpau -> "vdpau"
                | `Cuda -> "cuda"
                | `Vaapi -> "vaapi"
                | `Dxva2 -> "dxva2"
                | `Qsv -> "qsv"
                | `Videotoolbox -> "videotoolbox"
                | `D3d11va -> "d3d11va"
                | `Drm -> "drm"
                | `Opencl -> "opencl"
                | `Mediacodec -> "mediacodec"
                | _ -> "not supported in this test!"))
          configs));
  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let width = 352 in
  let height = 288 in

  let encode ?(pixel_format = `Yuv420p) ?hardware_context () =
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
      let string_of_comp
          { Avutil.Pixel_format.plane; step; shift; offset; depth } =
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

    let frame_rate = { Avutil.num = 25; den = 1 } in
    let time_base = { Avutil.num = 1; den = 25 } in
    let pts = ref 0L in

    let ovs =
      Av.new_video_stream ?hardware_context ~width ~height ~frame_rate
        ~time_base ~pixel_format ~codec:(codec ()) dst
    in

    let frame = Video.create_frame width height `Yuv420p in

    for i = 0 to 240 do
      Video.frame_visit ~make_writable:true
        (fill_yuv_image width height i)
        frame
      |> fun frame ->
      Avutil.Frame.set_pts frame (Some !pts);
      pts := Int64.succ !pts;
      Av.write_frame ovs frame
    done;

    Av.close dst;

    Gc.full_major ();
    Gc.full_major ()
  in
  try
    if mode = "device" then (
      let device_method =
        List.find_opt
          (fun { Avcodec.methods; _ } -> List.mem `Hw_device_ctx methods)
          configs
      in
      match device_method with
        | Some { Avcodec.device_type; _ } ->
            Printf.printf "Trying device context method..\n%!";
            let device_context =
              Avutil.HwContext.create_device_context device_type
            in
            encode ~hardware_context:(`Device_context device_context) ()
        | None ->
            Printf.printf "No device context method found for codec %s..\n%!"
              codec_name);
    if mode = "frame" then (
      let frame_method =
        List.find_opt
          (fun { Avcodec.methods; _ } -> List.mem `Hw_frames_ctx methods)
          configs
      in
      match frame_method with
        | Some { Avcodec.device_type; pixel_format; _ } ->
            Printf.printf "Trying frame context method..\n%!";
            let device_context =
              Avutil.HwContext.create_device_context device_type
            in
            let frame_context =
              Avutil.HwContext.create_frame_context ~width ~height
                ~src_pixel_format:`Yuv420p ~dst_pixel_format:pixel_format
                device_context
            in
            encode ~pixel_format
              ~hardware_context:(`Frame_context frame_context) ()
        | None ->
            Printf.printf "No frame context method found for codec %s..\n%!"
              codec_name)
  with exn ->
    Printf.printf "Optional test failed: %s\n%!" (Printexc.to_string exn)
