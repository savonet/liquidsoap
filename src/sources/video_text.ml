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

let () = Lang.add_module "video.text"

class text ~kind init render_text ttf ttf_size color duration text =
  let () = init () in
  object (self)
    inherit Synthesized.source ~seek:true ~name:"video.text" kind duration
    val mutable text_frame = None
    val mutable font = None
    val mutable cur_text = text ()

    method private render_text text =
      let w, h, get_pixel_rgba = render_text ~font:ttf ~size:ttf_size text in
      let tf = Video.Image.create w h in
      let tr, tg, tb = Image.RGB8.Color.of_int color in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let a = get_pixel_rgba x y in
          Image.YUV420.set_pixel_rgba tf x y (tr, tg, tb, a)
        done
      done;
      let tf = Video.Canvas.Image.make tf in
      text_frame <- Some tf

    method get_text_frame =
      match text_frame with
        | Some tf -> tf
        | None ->
            self#render_text cur_text;
            Option.get text_frame

    method private synthesize frame off len =
      let off = Frame.video_of_main off in
      let len = Frame.video_of_main len in
      let text = text () in
      if cur_text <> text then (
        cur_text <- text;
        self#render_text cur_text);
      let tf = self#get_text_frame in
      let buf = VFrame.data frame in
      for i = off to off + len - 1 do
        let img = buf.(i) in
        let width = Video.Canvas.Image.width img in
        let height = Video.Canvas.Image.height img in
        buf.(i) <- Video.Canvas.Image.viewport width height tf
      done
  end

let register name init render_text =
  let kind =
    { Frame.audio = `Any; video = Frame.video_yuva420p; midi = `Any }
  in
  let k = Lang.kind_type_of_kind_format kind in
  let add_operator op =
    Lang.add_operator op
      [
        ( "font",
          Lang.nullable_t Lang.string_t,
          Some Lang.null,
          Some "Path to ttf font file." );
        ("size", Lang.int_t, Some (Lang.int 18), Some "Font size.");
        ( "color",
          Lang.int_t,
          Some (Lang.int 0xffffff),
          Some "Text color (in 0xRRGGBB format)." );
        ( "duration",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some "Duration in seconds (`null` means infinite)." );
        ("", Lang.getter_t Lang.string_t, None, Some "Text to display.");
      ]
      ~return_t:k ~category:`Video ~descr:"Display a text."
      (fun p ->
        let ttf =
          List.assoc "font" p |> Lang.to_option |> Option.map Lang.to_string
        in
        let ttf = Option.value ~default:Configure.default_font ttf in
        let ttf_size = List.assoc "size" p |> Lang.to_int in
        let color = List.assoc "color" p |> Lang.to_int in
        let duration =
          List.assoc "duration" p |> Lang.to_option |> Option.map Lang.to_float
        in
        let text = List.assoc "" p |> Lang.to_string_getter in
        let kind = Kind.of_kind kind in
        (new text ~kind init render_text ttf ttf_size color duration text
          :> Source.source))
  in
  add_operator ("video.text." ^ name)
