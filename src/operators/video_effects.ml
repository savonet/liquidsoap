(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Source

class effect ~kind effect (source:source) =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready
  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = VFrame.position buf in
    source#get buf ;
    let position = VFrame.position buf in
    let rgb = (VFrame.content buf offset).(0) in
      for i = offset to position - 1 do
        effect rgb.(i)
      done
end

let kind = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~video:1 ())

let () =
  Lang.add_operator "video.greyscale"
    [ "", Lang.source_t kind, None, None ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Convert video to greyscale."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new effect ~kind RGB.greyscale src)

let () =
  Lang.add_operator "video.sepia"
    [ "", Lang.source_t kind, None, None ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Convert video to sepia."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new effect ~kind RGB.sepia src)

let () =
  Lang.add_operator "video.invert"
    [ "", Lang.source_t kind, None, None ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Invert video."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new effect ~kind RGB.invert src)

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
         new effect ~kind (fun buf -> RGB.scale_opacity buf a) src)

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
         new effect ~kind RGB.blur_alpha src)

let () =
  Lang.add_operator "video.lomo"
    [ "", Lang.source_t kind, None, None ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Emulate the \"Lomo effect\"."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new effect ~kind RGB.lomo src)

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
       let color = RGB.rgb_of_int color in
         new effect ~kind (fun buf -> RGB.color_to_alpha buf color prec) src)

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
       let r,g,b = RGB.rgb_of_int color in
         new effect ~kind (fun buf -> RGB.fill buf (r, g, b, 0xff)) src)

let () =
  Lang.add_operator "video.scale"
    [
      "coef", Lang.float_t, Some (Lang.float 1.),
      Some "Scaling coefficient in both directions.";

      "coef_x", Lang.float_t, Some (Lang.float 1.), Some "x scaling";
      "coef_y", Lang.float_t, Some (Lang.float 1.), Some "y scaling";
      "offset_x", Lang.int_t, Some (Lang.int 1), Some "x offset";
      "offset_y", Lang.int_t, Some (Lang.int 1), Some "y offset";
      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Scale and translate video."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let c, cx, cy, ox, oy =
         Lang.to_float (f "coef"),
         Lang.to_float (f "coef_x"),
         Lang.to_float (f "coef_y"),
         Lang.to_int (f "offset_x"),
         Lang.to_int (f "offset_y")
       in
         new effect ~kind (fun buf -> RGB.affine buf (c*.cx) (c*.cy) ox oy) src)

let () =
  let effect a da buf =
    a := !a +. da;
    RGB.rotate buf !a
  in
  Lang.add_operator "video.rotate"
    [
      "angle", Lang.float_t, Some (Lang.float 0.),
      Some "Initial angle in radians.";

      "speed", Lang.float_t, Some (Lang.float 3.1416),
      Some "Rotation speed in radians per sec.";

      "", Lang.source_t kind, None, None
    ]
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.VideoProcessing
    ~descr:"Rotate video."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
       let angle = ref (Lang.to_float (f "angle")) in
       let speed = Lang.to_float (f "speed") in
       let da = speed /. float (Lazy.force Frame.video_rate) in
         new effect ~kind (effect angle da) src)
