(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let log = Log.make ["video"]

let cached_effect effect_ =
  let cache = ref None in
  fun args ->
    match !cache with
      | Some (old_args, result) when old_args = args -> result
      | _ ->
          let result = effect_ args in
          cache := Some (args, result);
          result

let rgb_of_int c =
  let c =
    if c < 0 || c > 0xffffff then (
      log#important
        "color 0x%x is greater than maximum assignable value 0xffffff" c;
      c land 0xffffff)
    else c
  in
  Image.RGB8.Color.of_int c

let yuv_of_int c = Image.Pixel.yuv_of_rgb (rgb_of_int c)

let proto_color =
  [
    ( "color",
      Lang.getter_t Lang.int_t,
      Some (Lang.int 0),
      Some "Color to fill the image with (0xRRGGBB)." );
    ( "alpha",
      Lang.getter_t Lang.float_t,
      Some (Lang.float 1.),
      Some
        "Transparency of the color between 0 and 1 (0 is fully transparent and \
         1 is fully opaque)." );
  ]

module Getter = struct
  type 'a t = unit -> 'a

  let map f x () = f (x ())
end

let color_arg p =
  let color =
    List.assoc "color" p |> Lang.to_int_getter |> Getter.map yuv_of_int
  in
  let alpha =
    List.assoc "alpha" p |> Lang.to_float_getter
    |> Getter.map (fun x -> int_of_float (x *. 255.))
  in
  (color, alpha)

class virtual base ~name (source : source) f =
  object
    inherit operator ~name [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method virtual content_type : Frame.content_type

    method private generate_frame =
      let c = source#get_mutable_content Frame.Fields.video in
      let buf = Content.Video.get_data c in
      let data = buf.Content.Video.data in
      let data =
        if data = [] then data
        else (
          let positions, images =
            List.fold_left
              (fun (positions, images) (pos, img) ->
                (pos :: positions, img :: images))
              ([], []) buf.Content.Video.data
          in
          let positions = List.rev positions in
          let video = Array.of_list (List.rev images) in
          f video 0 (List.length images);
          List.mapi (fun i pos -> (pos, Video.Canvas.get video i)) positions)
      in
      source#set_frame_data Frame.Fields.video Content.Video.lift_data
        { buf with Content.Video.data }
  end

class effect_ ~name (source : source) effect_ =
  object
    inherit
      base
        ~name source
        (fun buf off len -> Video.Canvas.iter effect_ buf off len)
  end

class effect_map ~name (source : source) effect_ =
  object
    inherit
      base
        ~name source
        (fun buf off len -> Video.Canvas.map effect_ buf off len)
  end

let return_t () =
  Lang.frame_t (Lang.univ_t ())
    (Frame.Fields.make ~video:(Format_type.video ()) ())

let video_alpha = Lang.add_module ~base:Modules.video "alpha"

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "greyscale"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Convert video to greyscale."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.greyscale" src Image.YUV420.Effect.greyscale)

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "sepia"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Convert video to sepia."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.sepia" src Image.YUV420.Effect.sepia)

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "invert"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Invert video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.invert" src Image.YUV420.Effect.invert)

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "hmirror"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Flip image horizontally."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.hmirror" src Image.YUV420.hmirror)

let video_opacity =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "opacity"
    [
      ( "",
        Lang.getter_t Lang.float_t,
        None,
        Some
          "Coefficient to scale opacity with: from 0 (fully transparent) to 1 \
           (fully opaque)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Scale opacity of video."
    (fun p ->
      let a = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new effect_ ~name:"video.opacity" src (fun buf ->
          Image.YUV420.Effect.Alpha.scale buf (a ())))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:video_alpha "remove"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Remove α channel."
    (fun p ->
      let src = Lang.to_source (List.assoc "" p) in
      new effect_ ~name:"video.alpha.remove" src (fun img ->
          Image.YUV420.fill_alpha img 0xff))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "fill"
    ([("", Lang.source_t return_t, None, None)] @ proto_color)
    ~return_t ~category:`Video ~descr:"Fill frame with a color."
    (fun p ->
      let f v = List.assoc v p in
      let c, a = color_arg p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.fill" src (fun buf ->
          Image.YUV420.fill buf (c ());
          Image.YUV420.fill_alpha buf (a ())))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "persistence"
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
      new effect_ ~name:"video.persistence" src (fun buf ->
          let duration = duration () in
          if duration > 0. then (
            let alpha = 1. -. (1. /. (duration *. fps)) in
            let alpha = int_of_float (255. *. alpha) in
            Image.YUV420.fill_alpha !prev alpha;
            Image.YUV420.add !prev buf;
            prev := Image.YUV420.copy buf)))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "add_rectangle"
    ([
       ( "x",
         Lang.getter_t Lang.int_t,
         Some (Lang.int 0),
         Some "Horizontal offset." );
       ( "y",
         Lang.getter_t Lang.int_t,
         Some (Lang.int 0),
         Some "Vertical offset." );
       ("width", Lang.getter_t Lang.int_t, None, Some "Width.");
       ("height", Lang.getter_t Lang.int_t, None, Some "Height.");
       ("", Lang.source_t return_t, None, None);
     ]
    @ proto_color)
    ~return_t ~category:`Video ~descr:"Draw a rectangle."
    (fun p ->
      let x = List.assoc "x" p |> Lang.to_int_getter in
      let y = List.assoc "y" p |> Lang.to_int_getter in
      let width = List.assoc "width" p |> Lang.to_int_getter in
      let height = List.assoc "height" p |> Lang.to_int_getter in
      let c, a = color_arg p in
      let src = List.assoc "" p |> Lang.to_source in
      let effect_ =
        cached_effect (fun (width, height, color, alpha) ->
            let r = Image.YUV420.create width height in
            Image.YUV420.fill r color;
            Image.YUV420.fill_alpha r alpha;
            r)
      in
      new effect_map ~name:"video.add_rectangle" src (fun buf ->
          let x = x () in
          let y = y () in
          let width = width () in
          let height = height () in
          let color = c () in
          let alpha = a () in
          let r = effect_ (width, height, color, alpha) in
          let r = Video.Canvas.Image.make ~x ~y ~width:(-1) ~height:(-1) r in
          Video.Canvas.Image.add r buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:video_alpha "of_color"
    [
      ( "precision",
        Lang.float_t,
        Some (Lang.float 0.2),
        Some
          "Precision in color matching (0. means match precisely the color and \
           1. means match every color)." );
      ( "color",
        Lang.int_t,
        Some (Lang.int 0),
        Some "Color which should be transparent (in 0xRRGGBB format)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Set a color to be transparent."
    (fun p ->
      let f v = List.assoc v p in
      let prec, color, src =
        ( Lang.to_float (f "precision"),
          Lang.to_int (f "color"),
          Lang.to_source (f "") )
      in
      let prec = int_of_float (prec *. 255.) in
      let color = yuv_of_int color in
      new effect_ ~name:"video.alpha.of_color" src (fun buf ->
          Image.YUV420.alpha_of_color buf color prec))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:video_alpha "movement"
    [
      ( "precision",
        Lang.float_t,
        Some (Lang.float 0.2),
        Some
          "Precision when comparing pixels to those of previous image (between \
           0 and 1)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video
    ~descr:
      "Make moving parts visible and non-moving parts transparent. A cheap way \
       to have a bluescreen."
    (fun p ->
      (* let precision = List.assoc "precision" p |> Lang.to_float in *)
      let src = List.assoc "" p |> Lang.to_source in
      let prev = ref None in
      new effect_ ~name:"video.alpha.movement" src (fun img ->
          (match !prev with
            | None -> ()
            | Some prev -> Image.YUV420.alpha_of_diff prev img (0xff * 2 / 10) 2);
          prev := Some img))

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
         new effect_ src Image.YUV420.Effect.Alpha.blur)
*)

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "lomo"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video ~descr:"Emulate the \"Lomo effect\"."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect_ ~name:"video.lomo" src Image.YUV420.Effect.lomo)

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "rotate"
    [
      ( "angle",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.),
        Some "Angle in radians." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Rotate video."
    (fun p ->
      let a = List.assoc "angle" p |> Lang.to_float_getter in
      let s = List.assoc "" p |> Lang.to_source in
      new effect_ ~name:"video.rotate" s (fun buf ->
          let x = Image.YUV420.width buf / 2 in
          let y = Image.YUV420.height buf / 2 in
          Image.YUV420.rotate (Image.YUV420.copy buf) x y (a ()) buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "resize"
    [
      ( "width",
        Lang.nullable_t (Lang.getter_t Lang.int_t),
        Some Lang.null,
        Some "Target width (`null` means original width)." );
      ( "height",
        Lang.nullable_t (Lang.getter_t Lang.int_t),
        Some Lang.null,
        Some "Target height (`null` means original height)." );
      ( "proportional",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Keep original proportions." );
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
      let proportional = Lang.to_bool (f "proportional") in
      let ox = Lang.to_int_getter (f "x") in
      let oy = Lang.to_int_getter (f "y") in
      let scaler = Video_converter.scaler () in
      new effect_map ~name:"video.resize" src (fun buf ->
          let owidth = Video.Canvas.Image.width buf in
          let oheight = Video.Canvas.Image.height buf in
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
          buf
          |> Video.Canvas.Image.resize ~scaler ~proportional width height
          |> Video.Canvas.Image.translate (ox ()) (oy ())))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:video_opacity "box"
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
      new effect_ ~name:"video.opacity.box" src (fun buf ->
          Image.YUV420.box_alpha buf (ox ()) (oy ()) (width ()) (height ())
            (alpha ())))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "translate"
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
      new effect_map ~name:"video.translate" src (fun buf ->
          Video.Canvas.Image.translate (dx ()) (dy ()) buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "scale"
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
      new effect_map ~name:"video.scale" src (fun buf ->
          let c = c () in
          let cx = c *. cx () in
          let cy = c *. cy () in
          let d = 1080 in
          let cx = int_of_float ((cx *. float d) +. 0.5) in
          let cy = int_of_float ((cy *. float d) +. 0.5) in
          let scaler = Video_converter.scaler () in
          let buf = Video.Canvas.Image.scale ~scaler (cx, d) (cy, d) buf in
          Video.Canvas.Image.translate (ox ()) (oy ()) buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "add_line"
    ([
       ( "",
         Lang.getter_t (Lang.product_t Lang.int_t Lang.int_t),
         None,
         Some "Start point." );
       ( "",
         Lang.getter_t (Lang.product_t Lang.int_t Lang.int_t),
         None,
         Some "End point." );
       ("", Lang.source_t return_t, None, None);
     ]
    @ proto_color)
    ~return_t ~category:`Video ~descr:"Draw a line on the video."
    (fun param ->
      let to_point_getter v =
        let v = Lang.to_getter v in
        fun () ->
          let x, y = v () |> Lang.to_product in
          (Lang.to_int x, Lang.to_int y)
      in
      let p = Lang.assoc "" 1 param |> to_point_getter in
      let q = Lang.assoc "" 2 param |> to_point_getter in
      let s = Lang.assoc "" 3 param |> Lang.to_source in
      let c, a = color_arg param in
      let effect_ =
        cached_effect (fun (r, g, b, a) ->
            Video.Canvas.Image.Draw.line (r, g, b, a) (p ()) (q ()))
      in
      new effect_map ~name:"video.add_line" s (fun buf ->
          let r, g, b = c () in
          let a = a () in
          let line = effect_ (r, g, b, a) in
          Video.Canvas.Image.add line buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "render"
    [
      ( "transparent",
        Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Make uncovered portions of the image transparent (they are black by \
           default)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video
    ~descr:"Render the video by computing the result of its canvas images."
    (fun p ->
      let transparent = List.assoc "transparent" p |> Lang.to_bool in
      let s = List.assoc "" p |> Lang.to_source in
      new effect_map ~name:"video.render" s (fun buf ->
          Video.Canvas.Image.rendered ~transparent buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "viewport"
    [
      ("x", Lang.int_t, Some (Lang.int 0), Some "Horizontal offset.");
      ("y", Lang.int_t, Some (Lang.int 0), Some "Vertical offset.");
      ( "width",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "Width (default is frame width)." );
      ( "height",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "height (default is frame height)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video ~descr:"Set the viewport for the current video."
    (fun p ->
      let x = List.assoc "x" p |> Lang.to_int in
      let y = List.assoc "y" p |> Lang.to_int in
      let width =
        List.assoc "width" p |> Lang.to_option |> Option.map Lang.to_int
      in
      let height =
        List.assoc "height" p |> Lang.to_option |> Option.map Lang.to_int
      in
      let s = List.assoc "" p |> Lang.to_source in
      let video_width, video_height = Frame.video_dimensions () in
      let width =
        match width with Some width -> width | None -> Lazy.force video_width
      in
      let height =
        match height with
          | Some height -> height
          | None -> Lazy.force video_height
      in
      new effect_map ~name:"video.viewport" s (fun buf ->
          Video.Canvas.Image.viewport ~x ~y width height buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "crop"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video
    ~descr:"Make the viewport of the current video match its bounding box."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new effect_map ~name:"video.crop" s (fun buf ->
          let (x, y), (w, h) = Video.Canvas.Image.bounding_box buf in
          Video.Canvas.Image.viewport ~x ~y w h buf))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:Modules.video "align"
    [
      ("left", Lang.bool_t, Some (Lang.bool false), Some "Align left.");
      ("right", Lang.bool_t, Some (Lang.bool false), Some "Align right.");
      ("top", Lang.bool_t, Some (Lang.bool false), Some "Align top.");
      ("bottom", Lang.bool_t, Some (Lang.bool false), Some "Align bottom.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:`Video
    ~descr:"Translate the video so that it is aligned on boundaries."
    (fun p ->
      let f x = List.assoc x p |> Lang.to_bool in
      let left = f "left" in
      let right = f "right" in
      let top = f "top" in
      let bottom = f "bottom" in
      let s = List.assoc "" p |> Lang.to_source in
      new effect_map ~name:"video.align" s (fun buf ->
          let (x, y), (w, h) = Video.Canvas.Image.bounding_box buf in
          let dx =
            if left then -x
            else if right then Video.Canvas.Image.width buf - w
            else 0
          in
          let dy =
            if top then -y
            else if bottom then Video.Canvas.Image.height buf - h
            else 0
          in
          Video.Canvas.Image.translate dx dy buf))

let _ =
  let return_t = return_t () in
  let buf = ref None in
  let int ?(default = 0) f =
    Option.map f !buf |> Option.value ~default |> Lang.int
  in
  Lang.add_operator ~base:Modules.video "info"
    [("", Lang.source_t return_t, None, None)]
    ~meth:
      [
        {
          name = "width";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Width of video.";
          value =
            (fun _ -> Lang.val_fun [] (fun _ -> int Video.Canvas.Image.width));
        };
        {
          name = "height";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Height of video.";
          value =
            (fun _ -> Lang.val_fun [] (fun _ -> int Video.Canvas.Image.height));
        };
        {
          name = "planes";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Number of planes in a video frame.";
          value =
            (fun _ -> Lang.val_fun [] (fun _ -> int Video.Canvas.Image.planes));
        };
        {
          name = "size";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Size of a video frame (in bytes).";
          value =
            (fun _ -> Lang.val_fun [] (fun _ -> int Video.Canvas.Image.size));
        };
      ]
    ~return_t ~category:`Video
    ~descr:
      "Compute various information about the video (dimension, size, etc.). \
       Those are accessible through the methods attached to the source."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new effect_map ~name:"video.info" s (fun b ->
          buf := Some b;
          b))

let _ =
  let return_t = return_t () in
  Lang.add_operator ~base:video_alpha "to_y"
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:`Video
    ~descr:
      "Convert the α channel to Y channel, thus converting opaque \
       (resp. transparent) pixels to bright (resp. dark) ones. This is useful \
       to observe the α channel."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new effect_ ~name:"video.alpha.to_y" s Image.YUV420.alpha_to_y)

let _ =
  let return_t = return_t () in
  let x = ref 0 in
  let y = ref 0 in
  let width = ref 0 in
  let height = ref 0 in
  Lang.add_operator ~base:Modules.video "bounding_box"
    [("", Lang.source_t return_t, None, None)]
    ~meth:
      [
        {
          Lang.name = "x";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "x offset of video.";
          value = (fun _ -> Lang.val_fun [] (fun _ -> Lang.int !x));
        };
        {
          name = "y";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "y offset of video.";
          value = (fun _ -> Lang.val_fun [] (fun _ -> Lang.int !y));
        };
        {
          name = "width";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Width of video.";
          value = (fun _ -> Lang.val_fun [] (fun _ -> Lang.int !width));
        };
        {
          name = "height";
          scheme = ([], Lang.fun_t [] Lang.int_t);
          descr = "Height of video.";
          value = (fun _ -> Lang.val_fun [] (fun _ -> Lang.int !height));
        };
      ]
    ~return_t ~category:`Video
    ~descr:
      "Retrieve the origin (methods `x` / `y`) and the dimensions (methods \
       `width` / `height`) of the bounding box of the video."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new effect_map ~name:"video.bounding_box" s (fun buf ->
          let (x', y'), (w, h) = Video.Canvas.Image.bounding_box buf in
          x := x';
          y := y';
          width := w;
          height := h;
          buf))
