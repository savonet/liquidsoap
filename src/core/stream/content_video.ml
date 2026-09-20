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
open Content_base

module Base = struct
  type ('a, 'b) content = {
    length : int;
    mutable params : 'a;
    mutable data : (int * 'b) list;
  }

  let make ?(length = 0) params = { length; params; data = [] }
  let length { length } = length

  let blit :
      'a 'b.
      copy:('b -> 'b) ->
      ('a, 'b) content ->
      int ->
      ('a, 'b) content ->
      int ->
      int ->
      unit =
   fun ~copy src src_pos dst dst_pos len ->
    (* No compatibility check here, it's
       assumed to have been done beforehand. *)
    dst.params <- src.params;
    let data =
      List.filter
        (fun (pos, _) -> pos < dst_pos || dst_pos + len <= pos)
        dst.data
    in
    let src_end = src_pos + len in
    let data =
      List.fold_left
        (fun data (pos, p) ->
          if src_pos <= pos && pos < src_end then (
            let pos = dst_pos + (pos - src_pos) in
            (pos, copy p) :: data)
          else data)
        data (List.rev src.data)
    in
    dst.data <-
      List.stable_sort (fun (pos, _) (pos', _) -> compare pos pos') data

  let fill :
      'a 'b. ('a, 'b) content -> int -> ('a, 'b) content -> int -> int -> unit =
   fun src src_pos dst dst_pos len ->
    blit ~copy:(fun x -> x) src src_pos dst dst_pos len

  let copy ~copy { length; data; params } =
    { length; data = List.map (fun (pos, x) -> (pos, copy x)) data; params }

  let params { params } = params
end

(* The settings are core's, and this is where the two meet. *)
let () = Video_format.default_dimensions := Frame_settings.video_dimensions

module Data = struct
  open Frame_settings
  open Video_format.Specs
  include Base

  type params = Video_format.Specs.params
  type data = (params, Video.Canvas.image) content

  let make ?(length = 0) params =
    let default_width, default_height = video_dimensions () in
    let width = !!(Option.value ~default:default_width params.width) in
    let height = !!(Option.value ~default:default_height params.height) in
    let interval = main_of_video 1 in
    let img = Video.Canvas.Image.create width height in
    let data =
      List.init (video_of_main length) (fun i -> (i * interval, img))
    in
    { length; params; data }

  let blit = fill

  let copy : 'a. ('a, 'b) content -> ('a, 'b) content =
   fun src -> copy ~copy:(fun x -> x) src

  let checksum d =
    (* Hash the video frame positions and render each frame to get pixel data *)
    let frames_info =
      List.map
        (fun (pos, img) ->
          let width = Video.Canvas.Image.width img in
          let height = Video.Canvas.Image.height img in
          let rendered = Video.Canvas.Image.render img in
          let y = Image.YUV420.y rendered in
          let y_stride = Image.YUV420.y_stride rendered in
          (* Sample a few pixels from the Y plane for the checksum *)
          let samples = Buffer.create 256 in
          let y_len = Bigarray.Array1.dim y in
          for i = 0 to min 63 (y_len - 1) do
            let idx = i * y_len / 64 in
            Buffer.add_uint8 samples y.{idx}
          done;
          Printf.sprintf "%d:%dx%d:%d:%s" pos width height y_stride
            (Buffer.contents samples))
        d.data
    in
    Digest.string (String.concat "|" frames_info) |> Digest.to_hex
end

include MkDataBase (Video_format.Format) (Data)
include Video_format

let dimensions_of_format p =
  let p = get_params p in
  let default_width, default_height = Frame_settings.video_dimensions () in
  let width =
    Lazy.Mutexed.force (Option.value ~default:default_width p.width)
  in
  let height =
    Lazy.Mutexed.force (Option.value ~default:default_height p.height)
  in
  (width, height)

let alpha_of_format p = Unifier.deref (get_params p).alpha = Some true

let lift_canvas ?(offset = 0) ?length data =
  let interval = Frame_settings.main_of_video 1 in
  let data = Array.(to_list (mapi (fun pos d -> (pos * interval, d)) data)) in
  let params =
    match data with
      | [] -> { Specs.width = None; height = None; alpha = Unifier.make None }
      | (_, i) :: _ ->
          {
            Specs.width =
              Some (Lazy.Mutexed.from_val (Video.Canvas.Image.width i));
            height = Some (Lazy.Mutexed.from_val (Video.Canvas.Image.height i));
            alpha = Unifier.make (Some (Video.Canvas.Image.has_alpha i));
          }
  in
  let length =
    match length with Some l -> l | None -> List.length data * interval
  in
  let data =
    List.filter (fun (pos, _) -> offset <= pos && pos < offset + length) data
  in
  lift_data ~length { length; params; data }

let get_canvas data =
  let { Base.data } = get_data data in
  Array.of_list (List.map snd data)
