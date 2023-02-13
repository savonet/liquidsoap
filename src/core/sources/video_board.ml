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
    val mutable last_point = None
    method image = img
    method width = Image.YUV420.width img
    method height = Image.YUV420.height img

    method clear =
      Image.YUV420.blank img;
      last_point <- None

    method set_pixel (x, y) c =
      last_point <- Some (x, y);
      try Image.YUV420.set_pixel_rgba img x y c
      with Image.Invalid_position -> ()

    method line_to (x, y) (r, g, b) =
      match last_point with
        | None -> self#set_pixel (x, y) (r, g, b, 0xff)
        | Some (x', y') ->
            Image.Draw.line
              (fun x y -> self#set_pixel (x, y) (r, g, b, 0xff))
              (x', y') (x, y);
            last_point <- Some (x, y)

    method private synthesize frame off len =
      let frame_width, frame_height = self#video_dimensions in
      let off = Frame.video_of_main off in
      let len = Frame.video_of_main len in
      let buf = VFrame.data frame in
      for i = off to off + len - 1 do
        buf.(i) <-
          Video.Canvas.Image.make ~width:frame_width ~height:frame_height img
      done
  end

let _ =
  let clear b =
    Lang.val_fun [] (fun _ ->
        b#clear;
        Lang.unit)
  in
  let clear_and_copy b =
    Lang.val_fun
      [("x", "x", Some (Lang.int 0)); ("y", "y", Some (Lang.int 0))]
      (fun p ->
        let x = List.assoc "x" p |> Lang.to_int in
        let y = List.assoc "y" p |> Lang.to_int in
        let img = Image.YUV420.copy b#image in
        b#clear;
        Image.YUV420.add img ~x ~y b#image;
        Lang.unit)
  in
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
  let line_to board =
    Lang.val_fun
      [
        ("color", "c", Some (Lang.int 0xffffff));
        ("", "x", None);
        ("", "y", None);
      ]
      (fun p ->
        let r, g, b =
          List.assoc "c" p |> Lang.to_int |> Image.RGB8.Color.of_int
        in
        let x = List.assoc "x" p |> Lang.to_int in
        let y = List.assoc "y" p |> Lang.to_int in
        board#line_to (x, y) (r, g, b);
        Lang.unit)
  in
  let pixel board =
    Lang.val_fun
      [("", "x", None); ("", "y", None)]
      (fun p ->
        let x = List.assoc "x" p |> Lang.to_int in
        let y = List.assoc "y" p |> Lang.to_int in
        Lang.reference
          (fun () ->
            let img = board#image in
            let r, g, b, _ = Image.YUV420.get_pixel_rgba img x y in
            let c = Image.RGB8.Color.to_int (r, g, b) in
            Lang.int c)
          (fun c ->
            let r, g, b = c |> Lang.to_int |> Image.RGB8.Color.of_int in
            board#set_pixel (x, y) (r, g, b, 0xff)))
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
        ("clear", ([], Lang.fun_t [] Lang.unit_t), "Clear the board.", clear);
        ( "clear_and_copy",
          ( [],
            Lang.fun_t
              [(true, "x", Lang.int_t); (true, "y", Lang.int_t)]
              Lang.unit_t ),
          "Clear the board and copy the old board to the new one, translated.",
          clear_and_copy );
        ( "fill",
          ([], Lang.fun_t [(false, "", Lang.int_t)] Lang.unit_t),
          "Fill with given color (0xRRGGBB).",
          fill );
        ( "height",
          ([], Lang.fun_t [] Lang.int_t),
          "Current height of the board.",
          fun b -> Lang.val_fun [] (fun _ -> Lang.int b#height) );
        ( "line_to",
          ( [],
            Lang.fun_t
              [
                (true, "color", Lang.int_t);
                (false, "", Lang.int_t);
                (false, "", Lang.int_t);
              ]
              Lang.unit_t ),
          "Draw a line from the last point.",
          line_to );
        ( "width",
          ([], Lang.fun_t [] Lang.int_t),
          "Current width of the board.",
          fun b -> Lang.val_fun [] (fun _ -> Lang.int b#width) );
        ( "pixel",
          ( [],
            Lang.fun_t
              [(false, "", Lang.int_t); (false, "", Lang.int_t)]
              (Lang.ref_t Lang.int_t) ),
          "Retrieve a pixel whose contents is a color (in 0xRRGGBB format).",
          pixel );
      ]
    (fun p ->
      let width =
        List.assoc "width" p |> Lang.to_option |> Option.map Lang.to_int
      in
      let height =
        List.assoc "height" p |> Lang.to_option |> Option.map Lang.to_int
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
      Printf.printf "image: %dx%d\nx%!" width height;
      Image.YUV420.blank img;
      new board img ())
