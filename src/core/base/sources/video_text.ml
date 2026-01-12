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

let video_text = Lang.add_module ~base:Modules.video "text"

class text init render_text ttf ttf_size color duration text =
  let () = init () in
  object (self)
    inherit Synthesized.source ~seek:true ~name:"video.text" duration
    val mutable text_frame = None
    val mutable font = None
    val mutable cur_ttf = ttf ()
    val mutable cur_ttf_size = ttf_size ()
    val mutable cur_color = color ()
    val mutable cur_text = text ()

    method private render_text =
      let w, h, get_pixel_rgba =
        render_text ~font:cur_ttf ~size:cur_ttf_size cur_text
      in
      let tf = Video.Image.create w h in
      let tr, tg, tb = Video_effects.rgb_of_int cur_color in
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
            self#render_text;
            Option.get text_frame

    method private synthesize length =
      let frame = Frame.create ~length Frame.Fields.empty in
      let ttf = ttf () in
      let ttf_size = ttf_size () in
      let color = color () in
      let text = text () in
      if
        cur_ttf <> ttf || cur_ttf_size <> ttf_size || cur_color <> color
        || cur_text <> text
      then (
        cur_ttf <- ttf;
        cur_ttf_size <- ttf_size;
        cur_color <- color;
        cur_text <- text;
        self#render_text);
      let tf = self#get_text_frame in
      let create ~pos:_ ~width ~height () =
        Video.Canvas.Image.viewport width height tf
      in
      let buf = self#generate_video ~field:Frame.Fields.video ~create length in
      Frame.set_data frame Frame.Fields.video Content.Video.lift_data buf
  end

let register name init render_text =
  let add_operator op =
    let return_t =
      Lang.frame_t (Lang.univ_t ())
        (Frame.Fields.make ~video:(Format_type.video ()) ())
    in
    Lang.add_operator ~base:video_text op
      [
        ( "font",
          Lang.nullable_t (Lang.getter_t Lang.string_t),
          Some Lang.null,
          Some
            (Printf.sprintf "Path to ttf font file (default is `\"%s\"`)."
               Configure.conf_default_font#get) );
        ("size", Lang.getter_t Lang.int_t, Some (Lang.int 18), Some "Font size.");
        ( "color",
          Lang.getter_t Lang.int_t,
          Some (Lang.hex_int 0xffffff),
          Some "Text color (in 0xRRGGBB format)." );
        ( "duration",
          Lang.nullable_t Lang.float_t,
          Some Lang.null,
          Some "Duration in seconds (`null` means infinite)." );
        ("", Lang.getter_t Lang.string_t, None, Some "Text to display.");
      ]
      ~return_t ~category:`Video ~descr:"Display a text."
      (fun p ->
        let ttf =
          List.assoc "font" p |> Lang.to_option
          |> Option.map Lang.to_string_getter
          |> Option.value ~default:(fun () -> Configure.conf_default_font#get)
        in
        let ttf_size = List.assoc "size" p |> Lang.to_int_getter in
        let color = List.assoc "color" p |> Lang.to_int_getter in
        let duration =
          List.assoc "duration" p |> Lang.to_option |> Option.map Lang.to_float
        in
        let text = List.assoc "" p |> Lang.to_string_getter in
        (new text init render_text ttf ttf_size color duration text
          :> Source.source))
  in
  ignore (add_operator name)
