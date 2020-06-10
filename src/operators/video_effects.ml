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

class effect ~name ~kind effect (source : source) =
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
        | None -> ()
        | Some (rgb, offset, length) ->
            Array.iter (fun rgb -> Video.iter effect rgb offset length) rgb
  end

let kind = Lang.any
let return_t = Lang.kind_type_of_kind_format Lang.any

let () =
  let name = "video.greyscale" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:Lang.VideoProcessing
    ~descr:"Convert video to greyscale."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind Video.Image.Effect.greyscale src)

let () =
  let name = "video.sepia" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:Lang.VideoProcessing ~descr:"Convert video to sepia."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind Video.Image.Effect.sepia src)

let () =
  let name = "video.invert" in
  Lang.add_operator name
    [("", Lang.source_t return_t, None, None)]
    ~return_t ~category:Lang.VideoProcessing ~descr:"Invert video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new effect ~name ~kind Video.Image.Effect.invert src)

let () =
  let name = "video.opacity" in
  Lang.add_operator name
    [
      ( "",
        Lang.float_getter_t (),
        None,
        Some "Coefficient to scale opacity with." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:Lang.VideoProcessing ~descr:"Scale opacity of video."
    (fun p ->
      let a = Lang.to_float_getter (Lang.assoc "" 1 p) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new effect
        ~name ~kind
        (fun buf -> Video.Image.Effect.Alpha.scale buf (a ()))
        src)

let () =
  let name = "video.fill" in
  Lang.add_operator name
    [
      ( "color",
        Lang.int_getter_t (),
        Some (Lang.int 0),
        Some "Color to fill the image with (0xRRGGBB)." );
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:Lang.VideoProcessing ~descr:"Fill frame with a color."
    (fun p ->
      let f v = List.assoc v p in
      let color = Lang.to_int_getter (f "color") in
      let src = Lang.to_source (f "") in
      let color () =
        Image.Pixel.yuv_of_rgb (Image.RGB8.Color.of_int (color ()))
      in
      new effect
        ~name ~kind
        (fun buf ->
          Image.YUV420.fill buf (color ());
          Image.YUV420.fill_alpha buf 0xff)
        src)

(*
let () =
  Lang.add_operator "video.opacity.blur"
    [
      "", Lang.source_t kind, None, None
    ]
    ~return_t
    ~category:Lang.VideoProcessing
    ~descr:"Blur opacity of video."
    (fun p ->
       let src = Lang.to_source (Lang.assoc "" 1 p) in
         new effect ~kind Image.Effect.Alpha.blur src)

let () =
  Lang.add_operator "video.lomo"
    [ "", Lang.source_t kind, None, None ]
    ~return_t
    ~category:Lang.VideoProcessing
    ~descr:"Emulate the \"Lomo effect\"."
    (fun p ->
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
    ~return_t
    ~category:Lang.VideoProcessing
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
    ~category:Lang.VideoProcessing
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
      new effect ~kind effect src)
 *)

let () =
  let name = "video.resize" in
  Lang.add_operator name
    [
      ("width", Lang.int_getter_t (), Some (Lang.int (-1)), Some "Target width.");
      ( "height",
        Lang.int_getter_t (),
        Some (Lang.int (-1)),
        Some "Target height." );
      ("x", Lang.int_getter_t (), Some (Lang.int 0), Some "x offset.");
      ("y", Lang.int_getter_t (), Some (Lang.int 0), Some "y offset.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:Lang.VideoProcessing
    ~descr:"Resize and translate video."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let width = Lang.to_int_getter (f "width") in
      let height = Lang.to_int_getter (f "height") in
      let ox = Lang.to_int_getter (f "x") in
      let oy = Lang.to_int_getter (f "y") in
      new effect
        ~name ~kind
        (fun buf ->
          let width = width () in
          let height = height () in
          let width, height =
            if width >= 0 && height >= 0 then (width, height)
            else (
              (* Negative values mean proportional scale. *)
              let owidth = Video.Image.width buf in
              let oheight = Video.Image.height buf in
              if width < 0 && height < 0 then (owidth, oheight)
              else if width < 0 then (owidth * height / oheight, height)
              else if height < 0 then (width, oheight * width / owidth)
              else assert false )
          in
          let dst = Video.Image.create width height in
          Video.Image.scale buf dst;
          Video.Image.blank buf;
          Video.Image.fill_alpha buf 0;
          Video.Image.add dst ~x:(ox ()) ~y:(oy ()) buf)
        src)

let () =
  let name = "video.scale" in
  Lang.add_operator name
    [
      ( "scale",
        Lang.float_getter_t (),
        Some (Lang.float 1.),
        Some "Scaling coefficient in both directions." );
      ("xscale", Lang.float_getter_t (), Some (Lang.float 1.), Some "x scaling.");
      ("yscale", Lang.float_getter_t (), Some (Lang.float 1.), Some "y scaling.");
      ("x", Lang.int_getter_t (), Some (Lang.int 0), Some "x offset.");
      ("y", Lang.int_getter_t (), Some (Lang.int 0), Some "y offset.");
      ("", Lang.source_t return_t, None, None);
    ]
    ~return_t ~category:Lang.VideoProcessing ~descr:"Scale and translate video."
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
      new effect
        ~name ~kind
        (fun buf ->
          let round x = int_of_float (x +. 0.5) in
          let width =
            Video.Image.width buf |> float_of_int
            |> ( *. ) (c () *. cx ())
            |> round
          in
          let height =
            Video.Image.height buf |> float_of_int
            |> ( *. ) (c () *. cy ())
            |> round
          in
          let dst = Video.Image.create width height in
          Video.Image.scale buf dst;
          Video.Image.blank buf;
          Video.Image.fill_alpha buf 0;
          Video.Image.add dst ~x:(ox ()) ~y:(oy ()) buf)
        src)
