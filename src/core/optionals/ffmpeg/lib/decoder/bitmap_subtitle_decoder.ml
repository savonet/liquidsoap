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

type active = { start : int; stop : int; canvas : Video.Canvas.Image.t }

(* Palette entries are native-endian 0xAARRGGBB words. *)
let palette_color palette i =
  let byte k = Bigarray.Array1.get palette ((4 * i) + k) in
  if Sys.big_endian then (byte 1, byte 2, byte 3, byte 0)
  else (byte 2, byte 1, byte 0, byte 3)

let rgba_of_pict (pict : Avutil.Subtitle.pict) =
  let planes, line_sizes = pict.planes in
  let indices = planes.(0) and palette = planes.(1) in
  let image = Image.RGBA32.create pict.w pict.h in
  for y = 0 to pict.h - 1 do
    for x = 0 to pict.w - 1 do
      let index = Bigarray.Array1.get indices ((y * line_sizes.(0)) + x) in
      Image.RGBA32.set_pixel_rgba image x y (palette_color palette index)
    done
  done;
  Image.YUV420.of_RGBA32 image

type placement = { scale_x : int * int; scale_y : int * int; x : int; y : int }

let placement ~width ~height (cw, ch) =
  let fw, fh = Helpers.Fit.fitted_size ~width ~height (cw, ch) in
  {
    scale_x = (fw, cw);
    scale_y = (fh, ch);
    x = (width - fw) / 2;
    y = (height - fh) / 2;
  }

let place p ~width ~height (pict : Avutil.Subtitle.pict) =
  let sx v = v * fst p.scale_x / snd p.scale_x in
  let sy v = v * fst p.scale_y / snd p.scale_y in
  let w = max 1 (sx pict.w) and h = max 1 (sy pict.h) in
  let source = rgba_of_pict pict in
  let scaled = Image.YUV420.create w h in
  Image.YUV420.ensure_alpha scaled;
  Image.YUV420.scale source scaled;
  Video.Canvas.Image.make ~width ~height
    ~x:(p.x + sx pict.x)
    ~y:(p.y + sy pict.y)
    scaled

let create ~output ~faults ~field ~width ~height params =
  let kind = Frame.Fields.string_of_field field in
  let main = Time_base.main () and milli = { Avutil.num = 1; den = 1000 } in
  let canvas_size =
    match
      (Avcodec.Subtitle.get_width params, Avcodec.Subtitle.get_height params)
    with
      | cw, ch when cw > 0 && ch > 0 -> (cw, ch)
      | _ -> (width, height)
  in
  let placement = placement ~width ~height canvas_size in
  let video_rate = Lazy.Mutexed.force Frame.video_rate in
  let fps = { Decoder.num = video_rate; den = 1 } in
  let frame_ticks =
    Option.get (Time_base.rescale ~src:(Time_base.video ()) ~dst:main 1)
  in
  let active = ref [] in
  let pos = ref 0 in
  let advance r =
    let first = (!pos + frame_ticks - 1) / frame_ticks in
    let rec frames k =
      let t = k * frame_ticks in
      if t < r then (
        active := List.filter (fun a -> a.stop > t) !active;
        let image =
          List.fold_left
            (fun image a ->
              if a.start <= t then Video.Canvas.Image.add a.canvas image
              else image)
            (Video.Canvas.Image.create width height)
            !active
        in
        output.Per_stream.put_image ~fps image;
        frames (k + 1))
    in
    if r > !pos then (
      frames first;
      pos := r)
  in
  let to_main ms =
    Option.value ~default:0 (Time_base.rescale ~src:milli ~dst:main ms)
  in
  let push ~position (item : Per_stream.item) =
    match item with
      | `Subtitle_frame frame -> (
          let content = Avutil.Subtitle.get_content frame in
          let pts = Option.value ~default:!pos position in
          let canvases =
            List.filter_map
              (fun (r : Avutil.Subtitle.rectangle) ->
                match (r.rect_type, r.pict) with
                  | `Bitmap, Some pict ->
                      Some (place placement ~width ~height pict)
                  | _ -> None)
              content.rectangles
          in
          let skipped = List.length content.rectangles - List.length canvases in
          match canvases with
            | [] when skipped > 0 ->
                `Item_fault
                  (Printf.sprintf "Subtitle on %s dropped: no bitmap rectangle"
                     kind)
            | [] -> `Ok
            | first :: rest ->
                if skipped > 0 then
                  Faults.uncounted faults ~kind
                    (Printf.sprintf "%d non-bitmap subtitle rectangles skipped"
                       skipped);
                let canvas =
                  List.fold_left
                    (fun c r -> Video.Canvas.Image.add r c)
                    first rest
                in
                active :=
                  !active
                  @ [
                      {
                        start = pts + to_main content.start_display_time;
                        stop = pts + to_main content.end_display_time;
                        canvas;
                      };
                    ];
                Faults.delivered faults;
                `Ok)
      | _ -> `Item_fault "Unexpected item on a subtitle stream"
  in
  let flush () =
    advance (List.fold_left (fun m a -> max m a.stop) !pos !active)
  in
  {
    Per_stream.sparse = true;
    push;
    switch = (fun ~source_id:_ ~continuing:_ -> ());
    advance;
    flush;
  }
