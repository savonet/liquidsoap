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

(* TODO: Remove this. *)

type t = {
  mutable metadata : (int * Frame.metadata) list;
  mutable breaks : int list;
  mutable length : int;
}

let create () = { metadata = []; breaks = []; length = 0 }

let clear g =
  g.metadata <- [];
  g.breaks <- [];
  g.length <- 0

let advance g len =
  g.metadata <- List.map (fun (t, m) -> (t - len, m)) g.metadata;
  g.metadata <- List.filter (fun (t, _) -> t >= 0) g.metadata;
  g.breaks <- List.map (fun t -> t - len) g.breaks;
  g.breaks <- List.filter (fun t -> t >= 0) g.breaks;
  g.length <- g.length - len;
  assert (g.length >= 0)

let length g = g.length
let remaining g = match g.breaks with a :: _ -> a | _ -> -1
let metadata g len = List.filter (fun (t, _) -> t < len) g.metadata

let feed_from_frame g frame =
  let size = Lazy.force Frame.size in
  let length = length g in
  g.metadata <-
    g.metadata
    @ List.map (fun (t, m) -> (length + t, m)) (Frame.get_all_metadata frame);
  g.breaks <-
    g.breaks
    @ List.map
        (fun t -> length + t)
        (* Filter out the last break, which only marks the end of frame, not a
         * track limit (doesn't mean is_partial). *)
        (List.filter (fun x -> x < size) (Frame.breaks frame));
  let frame_length =
    let rec aux = function [t] -> t | _ :: tl -> aux tl | [] -> size in
    aux (Frame.breaks frame)
  in
  g.length <- g.length + frame_length

let drop_initial_break g =
  match g.breaks with
    | 0 :: tl -> g.breaks <- tl
    | [] -> () (* end of stream / underrun... *)
    | _ -> assert false

let fill g frame =
  let offset = Frame.position frame in
  let needed =
    let size = Lazy.force Frame.size in
    let remaining = remaining g in
    let remaining = if remaining = -1 then length g else remaining in
    min (size - offset) remaining
  in
  List.iter
    (fun (p, m) -> if p < needed then Frame.set_metadata frame (offset + p) m)
    g.metadata;
  advance g needed;

  (* Mark the end of this filling. If the frame is partial it must be because
   * of a break in the generator, or because the generator is emptying.
   * Conversely, each break in the generator must cause a partial frame, so
   * don't remove any if it isn't partial. *)
  Frame.add_break frame (offset + needed);
  if Frame.is_partial frame then drop_initial_break g
