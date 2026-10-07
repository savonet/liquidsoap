(* Conformance of swscale: spec/tests.md §7. *)

open Avutil
open Harness

external image_plane_sizes : Pixel_format.t -> int -> int -> int array
  = "test_image_plane_sizes"

let is_failure = function Error (`Failure _) -> true | _ -> false
let is_error = function Error _ -> true | _ -> false

module To_planes = Swscale.Make (Swscale.Frame) (Swscale.BigArray)
module To_packed = Swscale.Make (Swscale.Frame) (Swscale.PackedBigArray)
module To_strings = Swscale.Make (Swscale.Frame) (Swscale.Bytes)
module To_frame = Swscale.Make (Swscale.Frame) (Swscale.Frame)
module Planes_to_frame = Swscale.Make (Swscale.BigArray) (Swscale.Frame)
module Packed_to_frame = Swscale.Make (Swscale.PackedBigArray) (Swscale.Frame)
module Strings_to_frame = Swscale.Make (Swscale.Bytes) (Swscale.Frame)

let width = 320
and height = 240

let gradient ?(pixel_format = `Yuv420p) () =
  let frame = Video.create_frame width height pixel_format in
  ignore
    (Video.frame_visit ~make_writable:true
       (Array.iteri (fun plane (data, linesize) ->
            for i = 0 to Bigarray.Array1.dim data - 1 do
              data.{i} <-
                ((i mod linesize / 4) + (i / linesize / 4) + (plane * 40))
                land 0xff
            done))
       frame);
  frame

let frame_planes frame =
  let planes = ref [||] in
  ignore (Video.frame_visit ~make_writable:false (fun p -> planes := p) frame);
  !planes

let lengths planes =
  Array.map (fun (data, _) -> Bigarray.Array1.dim data) planes

let requirement_7_1 () =
  List.iter
    (fun format ->
      let out_width = 161 and out_height = 121 in
      let expected = image_plane_sizes format out_width out_height in
      equal
        (Pixel_format.planes format)
        (Array.length expected) "FFmpeg's plane count";
      let create make =
        make [Swscale.Bilinear] width height `Yuv420p out_width out_height
          format
      in
      let source = gradient () in
      let planes = create (To_planes.create ?threads:None) in
      equal expected
        (lengths (To_planes.convert planes source))
        "bigarray planes";
      equal expected
        (lengths (To_planes.convert planes source))
        "on a second call";
      let packed = create (To_packed.create ?threads:None) in
      let data, linesizes = To_packed.convert packed source in
      equal expected (Array.map Bigarray.Array1.dim data) "packed planes";
      equal (Array.length expected) (Array.length linesizes) "packed line sizes";
      let strings = create (To_strings.create ?threads:None) in
      equal expected
        (Array.map
           (fun (s, _) -> String.length s)
           (To_strings.convert strings source))
        "byte planes";
      equal expected
        (Array.map
           (fun (s, _) -> String.length s)
           (To_strings.convert strings source))
        "byte planes on a second call";
      let frames = create (To_frame.create ?threads:None) in
      let frame = To_frame.convert frames source in
      equal
        (out_width, out_height, format)
        Video.
          ( frame_get_width frame,
            frame_get_height frame,
            frame_get_pixel_format frame )
        "a frame of the output size and format";
      equal (Array.length expected)
        (Array.length (frame_planes frame))
        "its planes";
      equal None (Frame.pts frame) "a converted frame carries no timestamp")
    [`Gray8; `Yuv420p; `Yuv422p; `Nv12; `Rgb24];
  let from_palette =
    Planes_to_frame.create [Swscale.Bilinear] 16 16 `Pal8 16 16 `Rgb24
  in
  let palette = create_data 1024 and indexes = create_data 256 in
  Bigarray.Array1.fill palette 128;
  Bigarray.Array1.fill indexes 3;
  let frame =
    Planes_to_frame.convert from_palette [| (indexes, 16); (palette, 0) |]
  in
  equal 16
    (Video.frame_get_width frame)
    "a paletted input with its palette buffer";
  raises is_failure "a paletted input without its palette" (fun () ->
      Planes_to_frame.convert from_palette [| (indexes, 16) |]);
  raises is_failure "a paletted output for bigarrays" (fun () ->
      To_planes.create [Swscale.Bilinear] width height `Yuv420p width height
        `Pal8);
  raises is_failure "a paletted output for strings" (fun () ->
      To_strings.create [Swscale.Bilinear] width height `Yuv420p width height
        `Pal8);
  match
    To_frame.create [Swscale.Bilinear] width height `Yuv420p width height `Pal8
  with
    | scaler ->
        equal `Pal8
          (Video.frame_get_pixel_format (To_frame.convert scaler (gradient ())))
          "a paletted output for frames"
    | exception Error (`Failure _) ->
        check false "a paletted output is accepted for frames"
    | exception Error _ ->
        check true "libswscale itself refuses this paletted output"

let requirement_7_2 () =
  let original = Video.create_frame width height `Rgb24 in
  ignore
    (Video.frame_visit ~make_writable:true
       (fun planes ->
         let data, linesize = planes.(0) in
         for row = 0 to height - 1 do
           for column = 0 to width - 1 do
             let pixel = (row * linesize) + (column * 3) in
             data.{pixel} <- 40 + (column / 2);
             data.{pixel + 1} <- 40 + (row / 2);
             data.{pixel + 2} <- 120
           done
         done)
       original);
  let to_yuv =
    To_frame.create [Swscale.Bicubic] width height `Rgb24 width height `Yuv420p
  in
  let to_rgb =
    To_planes.create [Swscale.Bicubic] width height `Yuv420p width height `Rgb24
  in
  let data, linesize =
    (To_planes.convert to_rgb (To_frame.convert to_yuv original)).(0)
  in
  let reference, reference_linesize = (frame_planes original).(0) in
  let worst = ref 0 in
  for row = 0 to height - 1 do
    for byte = 0 to (width * 3) - 1 do
      let difference =
        abs
          (data.{(row * linesize) + byte}
          - reference.{(row * reference_linesize) + byte})
      in
      if difference > !worst then worst := difference
    done
  done;
  check (!worst <= 6)
    (Printf.sprintf "an image converted and back, within 6: %d" !worst);
  let solid = Video.create_frame width height `Rgb24 in
  ignore
    (Video.frame_visit ~make_writable:true
       (fun planes ->
         let data, _ = planes.(0) in
         for i = 0 to Bigarray.Array1.dim data - 1 do
           data.{i} <- [| 10; 200; 30 |].(i mod 3)
         done)
       solid);
  let rgb_to_yuv =
    To_frame.create [Swscale.Bilinear] width height `Rgb24 width height `Yuv420p
  in
  let yuv_to_rgb =
    To_planes.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Rgb24
  in
  let data, linesize =
    (To_planes.convert yuv_to_rgb (To_frame.convert rgb_to_yuv solid)).(0)
  in
  let pixel = (20 * linesize) + (30 * 3) in
  check
    (abs (data.{pixel} - 10) <= 4
    && abs (data.{pixel + 1} - 200) <= 4
    && abs (data.{pixel + 2} - 30) <= 4)
    "a solid colour stays that colour"

let requirement_7_3 () =
  let convert threads =
    let scaler =
      To_planes.create ~threads [Swscale.Bicubic] width height `Yuv420p 640 480
        `Rgb24
    in
    To_planes.convert scaler (gradient ())
  in
  check (convert 1 = convert 4) "several threads give the bytes of one thread"

let requirement_7_4 () =
  let scaler =
    To_frame.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Rgb24
  in
  raises is_failure "a frame of another size" (fun () ->
      To_frame.convert scaler (Video.create_frame 64 48 `Yuv420p));
  raises is_failure "a frame of another format" (fun () ->
      To_frame.convert scaler (gradient ~pixel_format:`Yuv422p ()));
  let from_planes =
    Planes_to_frame.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Rgb24
  in
  let planes = frame_planes (gradient ()) in
  equal 64
    (Video.frame_get_width (Planes_to_frame.convert from_planes planes))
    "valid planes";
  raises is_failure "too few buffers" (fun () ->
      Planes_to_frame.convert from_planes (Array.sub planes 0 2));
  raises is_failure "a short plane" (fun () ->
      let data, linesize = planes.(0) in
      let short =
        Bigarray.Array1.sub data 0 (Bigarray.Array1.dim data - linesize)
      in
      Planes_to_frame.convert from_planes
        [| (short, linesize); planes.(1); planes.(2) |]);
  raises is_failure "a short line size" (fun () ->
      let data, _ = planes.(0) in
      Planes_to_frame.convert from_planes
        [| (data, width / 2); planes.(1); planes.(2) |]);
  let from_packed =
    Packed_to_frame.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Rgb24
  in
  raises is_failure "packed arrays of different lengths" (fun () ->
      Packed_to_frame.convert from_packed (Array.map fst planes, [| 320; 160 |]));
  let from_strings =
    Strings_to_frame.create [Swscale.Bilinear] width height `Yuv420p 64 48
      `Rgb24
  in
  raises is_failure "a short string plane" (fun () ->
      Strings_to_frame.convert from_strings
        (Array.map (fun (_, linesize) -> (String.make 16 'x', linesize)) planes));
  raises is_failure "a width below 1" (fun () ->
      Swscale.create [] 0 height `Yuv420p 64 48 `Rgb24)

let requirement_7_5 () =
  let scaler =
    Swscale.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Rgb24
  in
  let source = frame_planes (gradient ()) in
  let reference = create_data (64 * 3 * 48) in
  Swscale.scale scaler source 0 height [| (reference, 192) |] 0;
  let offset = 10 in
  let destination = create_data (64 * 3 * (48 + offset)) in
  Bigarray.Array1.fill destination 0xaa;
  Swscale.scale scaler source 0 height [| (destination, 192) |] offset;
  check
    (Bigarray.Array1.sub destination (offset * 192) (48 * 192) = reference)
    "the image is written at the row offset";
  let untouched = ref true in
  for i = 0 to (offset * 192) - 1 do
    if destination.{i} <> 0xaa then untouched := false
  done;
  check !untouched "and nowhere else";
  raises is_failure "a slice outside the image" (fun () ->
      Swscale.scale scaler source 0 (height + 1) [| (reference, 192) |] 0);
  raises is_failure "a negative row" (fun () ->
      Swscale.scale scaler source (-1) height [| (reference, 192) |] 0);
  raises is_failure "a destination too short for the offset" (fun () ->
      Swscale.scale scaler source 0 height [| (reference, 192) |] offset);
  let to_yuv =
    Swscale.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Yuv420p
  in
  let plane rows linesize = (create_data (rows * linesize), linesize) in
  raises is_failure "an offset that splits a chroma row" (fun () ->
      Swscale.scale to_yuv source 0 height
        [| plane 49 64; plane 25 32; plane 25 32 |]
        1)

let requirement_7_6 () =
  raises
    (function Error (`Failure _) -> false | Error _ -> true | _ -> false)
    "a setting libswscale rejects"
    (fun () ->
      Swscale.create [Swscale.Bilinear] width height `Yuv420p 64 48 `Vaapi);
  check (Swscale.version.major >= 8) "the libswscale version"

let in_use = function Error (`Failure "Object in use!") -> true | _ -> false

let concurrent_use () =
  let scaler =
    To_planes.create [Swscale.Bicubic] width height `Yuv420p 640 480 `Rgb24
  in
  let source = gradient () in
  let reference = To_planes.convert scaler source in
  let failures = Atomic.make 0 and busy = Atomic.make 0 in
  let run () =
    for _ = 1 to 100 do
      match To_planes.convert scaler source with
        | planes -> if planes <> reference then Atomic.incr failures
        | exception exn when in_use exn -> Atomic.incr busy
        | exception _ -> Atomic.incr failures
    done
  in
  List.iter Thread.join (List.init 2 (fun _ -> Thread.create run ()));
  equal 0 (Atomic.get failures)
    "a conversion returns the image or raises the in-use error";
  check (Atomic.get busy > 0) "the guard was met at least once"

let requirements =
  [
    ("7.1", requirement_7_1);
    ("7.2", requirement_7_2);
    ("7.3", requirement_7_3);
    ("7.4", requirement_7_4);
    ("7.5", requirement_7_5);
    ("7.6", requirement_7_6);
    ("kc.swscale-concurrent-use", concurrent_use);
  ]
