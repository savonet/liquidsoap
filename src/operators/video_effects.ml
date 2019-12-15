(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

open Source

class effect ~kind effect (source : source) =
  object
    inherit operator ~name:"video.effect" kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method private get_frame buf =
      match VFrame.get_content buf source with
        | None -> ()
        | Some (rgb, offset, length) ->
            let rgb = rgb.(0) in
            Video.iter effect rgb offset length
  end

let kind = Lang.kind_type_of_kind_format (Lang.any_fixed_with ~video:1 ())

let () =
  Lang.add_operator "video.greyscale"
    [("", Lang.source_t kind, None, None)]
    ~kind:(Lang.Unconstrained kind) ~category:Lang.VideoProcessing
    ~descr:"Convert video to greyscale."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~kind Video.Image.Effect.greyscale src)

let () =
  Lang.add_operator "video.sepia"
    [("", Lang.source_t kind, None, None)]
    ~kind:(Lang.Unconstrained kind) ~category:Lang.VideoProcessing
    ~descr:"Convert video to sepia."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~kind Video.Image.Effect.sepia src)

let () =
  Lang.add_operator "video.invert"
    [("", Lang.source_t kind, None, None)]
    ~kind:(Lang.Unconstrained kind) ~category:Lang.VideoProcessing
    ~descr:"Invert video."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~kind Video.Image.Effect.invert src)

(*
let () =
  Lang.add_operator "video.opacity"
    [
      "", Lang.float_t, None, Some "Coefficient to scale opacity with.";
      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Scale opacity of video."
    (fun p kind ->
       let a = Lang.to_float (Lang.assoc "" 1 p) in
       let src = Lang.to_source (Lang.assoc "" 2 p) in
         new effect ~kind (fun buf -> Image.Effect.Alpha.scale buf a) src)

let () =
  Lang.add_operator "video.opacity.blur"
    [
      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Blur opacity of video."
    (fun p kind ->
       let src = Lang.to_source (Lang.assoc "" 1 p) in
         new effect ~kind Image.Effect.Alpha.blur src)

let () =
  Lang.add_operator "video.lomo"
    [ "", Lang.source_t kind, None, None ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Emulate the \"Lomo effect\"."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
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
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Set a color to be transparent."
    (fun p kind ->
       let f v = List.assoc v p in
       let prec, color, src =
         Lang.to_float (f "precision"),
         Lang.to_int (f "color"),
         Lang.to_source (f "")
       in
       let prec = int_of_float (prec *. 255.) in
       let color = Image.RGB8.Color.of_int color in
         new effect ~kind (fun buf -> Image.Effect.Alpha.of_color buf color prec) src)

let () =
  Lang.add_operator "video.fill"
    [
      "color", Lang.int_t, Some (Lang.int 0),
      Some "Color to fill the image with.";

      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Fill frame with a color."
    (fun p kind ->
       let f v = List.assoc v p in
       let color, src =
         Lang.to_int (f "color"),
         Lang.to_source (f "")
       in
       let r,g,b = Image.RGB8.Color.of_int color in
         new effect ~kind (fun buf -> Image.fill_all buf (r, g, b, 0xff)) src)

let () =
  Lang.add_operator "video.scale"
    [
      "scale", Lang.float_t, Some (Lang.float 1.),
      Some "Scaling coefficient in both directions.";
      "xscale", Lang.float_t, Some (Lang.float 1.), Some "x scaling.";
      "yscale", Lang.float_t, Some (Lang.float 1.), Some "y scaling.";
      "x", Lang.int_t, Some (Lang.int 0), Some "x offset.";
      "y", Lang.int_t, Some (Lang.int 0), Some "y offset.";
      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Scale and translate video."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let c, cx, cy, ox, oy =
         Lang.to_float (f "scale"),
         Lang.to_float (f "xscale"),
         Lang.to_float (f "yscale"),
         Lang.to_int (f "x"),
         Lang.to_int (f "y")
       in
         new effect ~kind
           (fun buf -> Image.Effect.affine buf (c*.cx) (c*.cy) ox oy) src)

let () =
  Lang.add_operator "video.rotate"
    [
      "angle", Lang.float_getter_t 1, Some (Lang.float 0.),
      Some "Initial angle in radians.";

      "speed", Lang.float_getter_t 2, Some (Lang.float Utils.pi),
      Some "Rotation speed in radians per sec.";

      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Rotate video."
    (fun p kind ->
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
      new effect ~kind effect src)
 *)
