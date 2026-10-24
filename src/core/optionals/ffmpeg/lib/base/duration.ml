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

type 'item accessors = {
  get_ts : 'item -> Int64.t option;
  set_ts : 'item -> Int64.t option -> unit;
  get_duration : 'item -> Int64.t option;
}

type 'item chunk = { length : int; items : 'item list }

type 'item result =
  [ `Chunk of 'item chunk | `Nothing | `Dropped | `Discontinuity of string ]

let max_group_items = 1024

type 'item t = {
  src : Time_base.t;
  dst : Time_base.t;
  convert_ts : bool;
  offset : int;
  start : int;
  faults : Faults.t;
  accessors : 'item accessors;
  mutable origin : int option;
  mutable last_ts : int option;
  (* Items of the held group, latest first. *)
  mutable group : 'item list;
  mutable group_size : int;
  mutable group_pos : int option;
  mutable group_end : int option;
  mutable last_length : int option;
  mutable emitted : int;
  mutable ended : bool;
}

let create ?(offset = 0) ?(start = 0) ~convert_ts ~src ~faults accessors =
  {
    src;
    dst = Time_base.main ();
    convert_ts;
    offset;
    start;
    faults;
    accessors;
    origin = None;
    last_ts = None;
    group = [];
    group_size = 0;
    group_pos = None;
    group_end = None;
    last_length = None;
    emitted = start;
    ended = false;
  }

let rescale t ts = Time_base.rescale ~src:t.src ~dst:t.dst ts

let group_full t =
  `Discontinuity
    (Printf.sprintf "%d items without an advancing timestamp" max_group_items)

let append t item =
  t.group <- item :: t.group;
  t.group_size <- t.group_size + 1

let emit t ~length =
  let chunk = { length; items = List.rev t.group } in
  t.group <- [];
  t.group_size <- 0;
  t.group_end <- None;
  t.last_length <- Some length;
  t.emitted <- t.emitted + length;
  chunk

let drop t message =
  match Faults.item t.faults ~kind:"timestamp" message with
    | `Item_fault -> `Dropped
    | `Source_fault ->
        `Discontinuity
          (Printf.sprintf "%d consecutive invalid timestamps"
             Faults.max_consecutive_faults)

let item_end t ~ts ~converted_origin item =
  match t.accessors.get_duration item with
    | None -> None
    | Some duration ->
        Option.map
          (fun e -> t.start + e - converted_origin)
          (rescale t (ts + Int64.to_int duration))

let push_timestamped t item ts =
  let ts = ts + t.offset in
  match t.last_ts with
    | Some last when ts < last ->
        drop t
          (Printf.sprintf "Decreasing timestamp %d after %d (time base %s)" ts
             last
             (Time_base.to_string t.src))
    | _ -> (
        let origin = Option.value ~default:ts t.origin in
        match (rescale t ts, rescale t origin) with
          | Some converted, Some converted_origin ->
              let p = t.start + converted - converted_origin in
              let closes =
                match t.group_pos with Some g -> p > g | None -> false
              in
              if (not closes) && t.group_size >= max_group_items then
                group_full t
              else (
                let item_end = item_end t ~ts ~converted_origin item in
                t.origin <- Some origin;
                t.accessors.set_ts item
                  (Some (Int64.of_int (if t.convert_ts then converted else ts)));
                t.last_ts <- Some ts;
                Faults.delivered t.faults;
                let result =
                  match t.group_pos with
                    | Some group_pos when closes ->
                        `Chunk (emit t ~length:(p - group_pos))
                    | _ -> `Nothing
                in
                t.group_pos <- Some p;
                t.group_end <-
                  (match (t.group_end, item_end) with
                    | Some e, Some e' -> Some (max e e')
                    | e, None | None, e -> e);
                append t item;
                result)
          | _ ->
              drop t
                (Printf.sprintf "Timestamp %d out of range converting from %s"
                   ts
                   (Time_base.to_string t.src)))

let push t item =
  if t.ended then failwith "Duration helper: push after flush";
  match t.accessors.get_ts item with
    | None when t.group_size >= max_group_items -> group_full t
    | None ->
        append t item;
        `Nothing
    | Some ts -> push_timestamped t item (Int64.to_int ts)

let flush t =
  t.ended <- true;
  match t.group with
    | [] -> None
    | _ ->
        let group_pos = Option.value ~default:t.start t.group_pos in
        let length =
          match (t.group_end, t.last_length) with
            | Some e, _ when e - group_pos > 0 -> e - group_pos
            | _, Some l when l > 0 -> l
            | _ -> 1
        in
        Some (emit t ~length)

let last_ts t = t.last_ts
let end_position t = t.emitted
