(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Mm
open Source

class virtual base ~name ~kind (source : source) f =
  object
    inherit operator ~name kind [source]
    method stype = source#stype
    method remaining = source#remaining
    method seek = source#seek
    method self_sync = source#self_sync
    method is_ready = source#is_ready
    method abort_track = source#abort_track

    method private get_frame buf =
      match VFrame.get_content buf source with
        | Some (rgb, offset, length) -> (
            try f (Content.Video.get_data rgb) offset length
            with Content.Invalid -> ())
        | _ -> ()
  end

class effect ~name ~kind (source : source) effect =
  object
    inherit
      base
        ~name ~kind source
        (fun buf off len -> Video.Canvas.iter effect buf off len)
  end

class effect_map ~name ~kind (source : source) effect =
  object
    inherit
      base
        ~name ~kind source
        (fun buf off len -> Video.Canvas.map effect buf off len)
  end

let kind = Kind.of_kind Lang.any
let return_t = Lang.kind_type_of_kind_format Lang.any

let () =
  let name = "video.greyscale" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Convert video to greyscale."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind src Image.YUV420.Effect.greyscale)

let () =
  let name = "video.sepia" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Convert video to sepia."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind src Image.YUV420.Effect.sepia)

let () =
  let name = "video.invert" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Invert video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind src Image.YUV420.Effect.invert)

let () =
  let name = "video.opacity" in
  Lang.add_operator name
    [
      ( "",
        Lang.getter_t Lang.float_t,
        None,
        Some "Coefficient to scale opacity with." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Scale opacity of video."
    (fun p ->
      let a = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new effect ~name ~kind src (fun buf ->
          Image.YUV420.Effect.Alpha.scale buf (a ())))

let () =
  let name = "video.fill" in
  Lang.add_operator name
    [
      ( "color",
        Lang.getter_t Lang.int_t,
        Some (Lang.int 0),
        Some "Color to fill the image with (0xRRGGBB)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Fill frame with a color."
    (fun p ->
      let f v = List.assoc v p in
      let color = Lang.to_int_getter (f "color") in
      let src = Lang.to_source (f "") in
      let color () =
        Image.Pixel.yuv_of_rgb (Image.RGB8.Color.of_int (color ()))
      in
      new effect ~name ~kind src (fun buf ->
          Image.YUV420.fill buf (color ());
          Image.YUV420.fill_alpha buf 0xff))

let () =
  let name = "video.persistence" in
  Lang.add_operator name
    [
      ( "duration",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Persistence duration in seconds." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Make images of the video persistent."
    (fun p ->
      let duration = List.assoc "duration" p |> Lang.to_float_getter in
      let src = List.assoc "" p |> Lang.to_source in
      let fps = Lazy.force Frame.video_rate |> float_of_int in
      let prev = ref (Image.YUV420.create 0 0) in
      new effect ~name ~kind src (fun buf ->
          let duration = duration () in
          if duration > 0. then (
            let alpha = 1. -. (1. /. (duration *. fps)) in
            let alpha = int_of_float (255. *. alpha) in
            Image.YUV420.fill_alpha !prev alpha;
            Image.YUV420.add !prev buf;
            prev := Image.YUV420.copy buf)))

let () =
  let name = "video.rectangle" in
  Lang.add_operator name
    [
      ( "x",
        Lang.getter_t Lang.int_t,
        Some (Lang.int 0),
        Some "Horizontal offset." );
      ("y", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "Vertical offset.");
      ("width", Lang.getter_t Lang.int_t, None, Some "Width.");
      ("height", Lang.getter_t Lang.int_t, None, Some "Height.");
      ( "color",
        Lang.getter_t Lang.int_t,
        Some (Lang.int 0),
        Some "Color to fill the image with (0xAARRGGBB)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Draw a rectangle."
    (fun p ->
      let x = List.assoc "x" p |> Lang.to_int_getter in
      let y = List.assoc "y" p |> Lang.to_int_getter in
      let width = List.assoc "width" p |> Lang.to_int_getter in
      let height = List.assoc "height" p |> Lang.to_int_getter in
      let color = List.assoc "color" p |> Lang.to_int_getter in
      let src = List.assoc "" p |> Lang.to_source in
      new effect ~name ~kind src (fun buf ->
          let x = x () in
          let y = y () in
          let width = width () in
          let height = height () in
          let c =
            color () |> Image.RGB8.Color.of_int |> Image.Pixel.yuv_of_rgb
          in
          let r = Image.YUV420.create width height in
          Image.YUV420.fill r c;
          Image.YUV420.add r ~x ~y buf))

(*
let () =
  Lang.add_operator "video.opacity.blur"
    [
      "", Lang.source_t return_t, None, None
    ]
    ~return_t
    ~category:`Video
    ~descr:"Blur opacity of video."
    (fun p ->
       let src = Lang.to_source (Lang.assoc "" 1 p) in
         new effect ~kind src Image.YUV420.Effect.Alpha.blur)

let () =
  Lang.add_operator "video.lomo"
    [ "", Lang.source_t kind, None, None ]
    ~return_t
    ~category:`Video
    ~descr:"Emulate the \"Lomo effect\"."
    (fun p ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let kind = Kind.of_kind kind in
       new effect ~kind Image.Effect.lomo src)

let () =
  Lang.add_operator "video.transparent"
    [
      "precision", Lang.float_t, Some (Lang.float 0.),
      Some "Precision in color matching (0. means match precisely the color \
            and 1. means match every color).";

      "color", Lang.int_t, Some (Lang.int 0),
      Some "Color which should be transparent (in 0xRRGGBB format).";

      "", Lang.source_t kind, None, None
    ]
    ~return_t
    ~category:`Video
    ~descr:"Set a color to be transparent."
    (fun p ->
       let f v = List.assoc v p in
       let prec, color, src =
         Lang.to_float (f "precision"),
         Lang.to_int (f "color"),
         Lang.to_source (f "")
       in
       let prec = int_of_float (prec *. 255.) in
       let color = Image.RGB8.Color.of_int color in
       let kind = Kind.of_kind kind in
       new effect ~kind (fun buf -> Image.Effect.Alpha.of_color buf color prec) src)

let () =
  Lang.add_operator "video.rotate"
    [
      "angle", Lang.float_getter_t 1, Some (Lang.float 0.),
      Some "Initial angle in radians.";

      "speed", Lang.float_getter_t 2, Some (Lang.float Utils.pi),
      Some "Rotation speed in radians per sec.";

      "", Lang.source_t kind, None, None
    ]
    ~return_t
    ~category:`Video
    ~descr:"Rotate video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let a = Lang.to_float_getter (f "angle") in
      let speed = Lang.to_float_getter (f "speed") in
      let da =
        let fps = float (Lazy.force Frame.video_rate) in
        fun () ->
          speed () /. fps
      in
      let angle = ref (a ()) in
      let a_old = ref (a ()) in
      let effect buf =
        if a () <> !a_old then
          (
            a_old := a ();
            angle := !a_old
          )
        else
          angle := !angle +. da ();
        Image.Effect.rotate buf !angle
      in
      let kind = Kind.of_kind kind in
      new effect ~kind effect src)
 *)

let () =
  let name = "video.resize" in
  Lang.add_operator name
    [
      ( "width",
        Lang.nullable_t (Lang.getter_t Lang.int_t),
        Some Lang.null,
        Some "Target width (`null` means original width)." );
      ( "height",
        Lang.nullable_t (Lang.getter_t Lang.int_t),
        Some Lang.null,
        Some "Target height (`null` means original height)." );
      ("x", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "x offset.");
      ("y", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "y offset.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Resize and translate video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let width = Lang.to_valued_option Lang.to_int_getter (f "width") in
      let height = Lang.to_valued_option Lang.to_int_getter (f "height") in
      let ox = Lang.to_int_getter (f "x") in
      let oy = Lang.to_int_getter (f "y") in
      new effect ~name ~kind src (fun buf ->
          let owidth = Video.Image.width buf in
          let oheight = Video.Image.height buf in
          let width = match width with None -> owidth | Some w -> w () in
          let height = match height with None -> oheight | Some h -> h () in
          let width, height =
            if width >= 0 && height >= 0 then (width, height)
            else if
              (* Negative values mean proportional scale. *)
              width < 0 && height < 0
            then (owidth, oheight)
            else if width < 0 then (owidth * height / oheight, height)
            else if height < 0 then (width, oheight * width / owidth)
            else assert false
          in
          let dst = Video.Image.create width height in
          Image.YUV420.scale buf dst;
          Video.Image.blank buf;
          Video.Image.fill_alpha buf 0;
          Video.Image.add dst ~x:(ox ()) ~y:(oy ()) buf))

let () =
  let name = "video.opacity.box" in
  Lang.add_operator name
    [
      ("width", Lang.getter_t Lang.int_t, None, Some "Box width.");
      ("height", Lang.getter_t Lang.int_t, None, Some "Box height.");
      ("x", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "x offset.");
      ("y", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "y offset.");
      ("alpha", Lang.getter_t Lang.float_t, None, Some "alpha value.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video
    ~descr:"Set alpha value on a given box inside the image."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let width = Lang.to_int_getter (f "width") in
      let height = Lang.to_int_getter (f "height") in
      let ox = Lang.to_int_getter (f "x") in
      let oy = Lang.to_int_getter (f "y") in
      let alpha = Lang.to_float_getter (f "alpha") in
      new effect ~name ~kind src (fun buf ->
          Image.YUV420.box_alpha buf (ox ()) (oy ()) (width ()) (height ())
            (alpha ())))

let () =
  let name = "video.translate" in
  Lang.add_operator name
    [
      ("x", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "x offset.");
      ("y", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "y offset.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Translate video."
    (fun p ->
      let f v = List.assoc v p in
      let src = f "" |> Lang.to_source in
      let dx = f "x" |> Lang.to_int_getter in
      let dy = f "y" |> Lang.to_int_getter in
      new effect_map ~name ~kind src (fun buf ->
          Video.Canvas.Image.translate (dx ()) (dy ()) buf))

let () =
  let name = "video.scale" in
  Lang.add_operator name
    [
      ( "scale",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "Scaling coefficient in both directions." );
      ( "xscale",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "x scaling." );
      ( "yscale",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 1.),
        Some "y scaling." );
      ("x", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "x offset.");
      ("y", Lang.getter_t Lang.int_t, Some (Lang.int 0), Some "y offset.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Scale and translate video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let c, cx, cy, ox, oy =
        ( Lang.to_float_getter (f "scale"),
          Lang.to_float_getter (f "xscale"),
          Lang.to_float_getter (f "yscale"),
          Lang.to_int_getter (f "x"),
          Lang.to_int_getter (f "y") )
      in
      new effect_map ~name ~kind src (fun buf ->
          let buf = Video.Canvas.Image.render buf in
          let round x = int_of_float (x +. 0.5) in
          let c = c () in
          let cx = c *. cx () in
          let cy = c *. cy () in
          let width =
            Video.Image.width buf |> float_of_int |> ( *. ) cx |> round
          in
          let height =
            Video.Image.height buf |> float_of_int |> ( *. ) cy |> round
          in
          let dst = Video.Image.create width height in
          Image.YUV420.scale buf dst;
          Video.Canvas.Image.make ~x:(ox ()) ~y:(oy ())
            ~width:(Lazy.force Frame.video_width)
            ~height:(Lazy.force Frame.video_height)
            dst))
