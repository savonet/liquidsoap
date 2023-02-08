(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2023 Savonet team

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

class board ?duration img0 () =
  object (self)
    inherit Synthesized.source ~seek:true ~name:"video.board" duration
    val mutable img = img0
    val mutable x = 0
    val mutable y = 0
    method image = img
    method get_x = x
    method set_x x' = x <- x'
    method get_y = y
    method set_y y' = y <- y'

    method private synthesize frame off len =
      let frame_width, frame_height = self#video_dimensions in
      let off = Frame.video_of_main off in
      let len = Frame.video_of_main len in
      let buf = VFrame.data frame in
      for i = off to off + len - 1 do
        buf.(i) <-
          Video.Canvas.Image.make ~x ~y ~width:frame_width ~height:frame_height
            img
      done
  end

let _ =
  let fill b =
    Lang.val_fun
      [("", "", None)]
      (fun p ->
        let c =
          List.assoc "" p |> Lang.to_int |> Image.RGB8.Color.of_int
          |> Image.Pixel.yuv_of_rgb
        in
        Image.YUV420.fill b#image c;
        Lang.unit)
  in
  let set_pixel b =
    Lang.val_fun
      [("color", "color", None); ("", "x", None); ("", "y", None)]
      (fun p ->
        let img = b#image in
        let r, g, b =
          List.assoc "color" p |> Lang.to_int |> Image.RGB8.Color.of_int
        in
        let x = List.assoc "x" p |> Lang.to_int in
        let y = List.assoc "y" p |> Lang.to_int in
        Image.YUV420.set_pixel_rgba img x y (r, g, b, 0xff);
        Lang.unit)
  in
  Lang.add_operator ~base:Modules.video "board"
    [
      ( "width",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "Initial width of the video (defaults ot the same as frame)." );
      ( "height",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "Initial height of the video (defaults ot the same as frame)." );
    ]
    ~return_t:(Lang.internal_t ()) ~category:`Video
    ~descr:"A plane where one can draw."
    ~meth:
      [
        ( "fill",
          ([], Lang.fun_t [(false, "", Lang.int_t)] Lang.unit_t),
          "Fill with given color (0xRRGGBB).",
          fill );
        ( "set_pixel",
          ( [],
            Lang.fun_t
              [
                (false, "color", Lang.int_t);
                (false, "", Lang.int_t);
                (false, "", Lang.int_t);
              ]
              Lang.unit_t ),
          "Set pixel to given color.",
          set_pixel );
        ( "x",
          ([], Lang.ref_t Lang.int_t),
          "Horizontal translation.",
          fun b ->
            Lang.reference
              (fun () -> Lang.int @@ b#get_x)
              (fun x' -> b#set_x @@ Lang.to_int @@ x') );
        ( "y",
          ([], Lang.ref_t Lang.int_t),
          "Vertical translation.",
          fun b ->
            Lang.reference
              (fun () -> Lang.int @@ b#get_y)
              (fun y' -> b#set_y @@ Lang.to_int @@ y') );
      ]
    (fun p ->
      let width =
        List.assoc "width" p |> Lang.to_option |> Option.map Lang.to_int
      in
      let height =
        List.assoc "width" p |> Lang.to_option |> Option.map Lang.to_int
      in
      let width =
        match width with
          | Some width -> width
          | None -> Lazy.force Frame.video_width
      in
      let height =
        match height with
          | Some height -> height
          | None -> Lazy.force Frame.video_height
      in
      let img = Video.YUV420.create width height in
      new board img ())
