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

class testsrc ?(duration = None) ~width ~height () =
  object (self)
    inherit Synthesized.source ~seek:true ~name:"video.testsrc" duration
    val mutable u0 = 0
    val mutable v0 = 0
    val mutable dux = 0
    val mutable dvx = 0
    val mutable duy = 0
    val mutable dvy = 0
    val mutable u0' = 2
    val mutable v0' = 3
    val mutable dux' = 5
    val mutable dvx' = 4
    val mutable duy' = 3
    val mutable dvy' = 2

    method private synthesize length =
      let frame = Frame.create ~length Frame.Fields.empty in
      let create ~pos:_ ~width:frame_width ~height:frame_height () =
        let width = if width < 0 then frame_width else width in
        let height = if height < 0 then frame_height else height in
        let img = Image.YUV420.create width height in
        u0 <- u0 + u0';
        if u0 < 0 then u0' <- abs u0';
        if u0 > 0x7f then u0' <- -abs u0';
        v0 <- v0 + v0';
        if v0 < 0 then v0' <- abs v0';
        if v0 > 0x7f then v0' <- -abs v0';
        dux <- dux + dux';
        if dux < 0x1f then dux' <- abs dux';
        if u0 + dux > 0x1ff then dux' <- -abs dux';
        dvx <- dvx + dvx';
        if dvx < 0x2f then dvx' <- abs dvx';
        if v0 + dvx > 0x1ff then dvx' <- -abs dvx';
        duy <- duy + duy';
        if duy < 0x3f then duy' <- abs duy';
        if u0 + duy > 0x1ff then duy' <- -abs duy';
        dvy <- dvy + dvy';
        if dvy < 0x4f then dvy <- abs dvy;
        if v0 + dvy > 0x1ff then dvy' <- -abs dvy';
        Image.YUV420.gradient_uv img (u0, v0)
          (u0 + dux, v0 + dvx)
          (u0 + duy, v0 + dvy);
        Video.Canvas.Image.make ~width:frame_width ~height:frame_height img
      in
      let buf = self#generate_video ~field:Frame.Fields.video ~create length in
      Frame.set_data frame Frame.Fields.video Content.Video.lift_data buf
  end

let _ =
  let return_t = Lang.internal_tracks_t () in
  Lang.add_operator ~base:Modules.video "testsrc" ~category:`Input
    ~descr:"Generate a test video."
    [
      ("width", Lang.int_t, Some (Lang.int (-1)), None);
      ("height", Lang.int_t, Some (Lang.int (-1)), None);
    ]
    ~return_t
    (fun p ->
      let width = List.assoc "width" p |> Lang.to_int in
      let height = List.assoc "height" p |> Lang.to_int in
      new testsrc ~width ~height ())
