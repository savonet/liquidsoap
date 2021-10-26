(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

type ('a, 'b) content = {
  mutable params : 'a;
  mutable size : int;
  mutable data : (int * 'b) list;
}

let make ~size params = { params; size; data = [] }
let clear d = d.data <- []
let is_empty { data } = data = []
let stream_idx = ref 0L

let new_stream_idx () =
  stream_idx := Int64.succ !stream_idx;
  !stream_idx

let sort : 'a. (int * 'a) list -> (int * 'a) list =
 fun data -> List.sort (fun (p, _) (p', _) -> Stdlib.compare p p') data

let sub c ofs len =
  {
    c with
    data = List.filter (fun (pos, _) -> ofs <= pos && pos < ofs + len) c.data;
  }

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
  let src_end = src_pos + len in
  dst.data <-
    sort
      (List.fold_left
         (fun data (pos, p) ->
           if src_pos <= pos && pos < src_end then (
             let pos = dst_pos + (pos - src_pos) in
             (pos, copy p) :: data)
           else data)
         dst.data src.data)

let fill :
      'a 'b. ('a, 'b) content -> int -> ('a, 'b) content -> int -> int -> unit =
 fun src src_pos dst dst_pos len ->
  blit ~copy:(fun x -> x) src src_pos dst dst_pos len

let copy ~copy d =
  { d with data = List.map (fun (pos, x) -> (pos, copy x)) d.data }

let params { params } = params
let length { size } = size
