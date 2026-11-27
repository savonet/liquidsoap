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

type 'item continuity = {
  max_drift : int;
  samples : 'item -> (int * int) option;
  trim : 'item -> int -> 'item;
}

(* Samples fed since [anchor], at [rate]. *)
type clock = { anchor : int; rate : int; fed : int }
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
  continuity : 'item continuity option;
  mutable clock : clock option;
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

let create ?(offset = 0) ?(start = 0) ?continuity ~convert_ts ~src ~faults
    accessors =
  {
    src;
    dst = Time_base.main ();
    convert_ts;
    offset;
    start;
    faults;
    accessors;
    continuity;
    clock = None;
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

let samples_length ~rate samples =
  Option.value ~default:0
    (Time_base.rescale
       ~src:{ Avutil.num = 1; den = rate }
       ~dst:(Time_base.main ()) samples)

let expected clock = clock.anchor + samples_length ~rate:clock.rate clock.fed

let drift_bound t continuity =
  max (Time_base.rounding_bound t.src) continuity.max_drift

let log_drift t drift =
  Faults.uncounted t.faults ~kind:"audio"
    (Printf.sprintf
       (if drift > 0 then "%.1f ms gap in audio timestamps"
        else "%.1f ms of audio ahead of its timestamps dropped")
       (1000. *. Frame.seconds_of_main (abs drift)))

(* common.md §6.7: within the drift bound, an audio item follows the samples
   before it. Past it, a later item keeps its position, leaving a gap, and an
   earlier one loses the samples that overlap those before it. *)
let place t ~continuity ~position item =
  let anchor ~rate position samples =
    t.clock <- Some { anchor = position; rate; fed = samples };
    Some (position, item)
  in
  match (continuity.samples item, t.clock) with
    | None, _ -> Some (position, item)
    | Some (samples, rate), None -> anchor ~rate position samples
    | Some (samples, rate), Some clock ->
        let expected = expected clock in
        let drift = position - expected in
        let follow item samples =
          t.clock <-
            Some
              (if clock.rate = rate then
                 { clock with fed = clock.fed + samples }
               else { anchor = expected; rate; fed = samples });
          Some (expected, item)
        in
        if abs drift <= drift_bound t continuity then follow item samples
        else (
          log_drift t drift;
          if drift > 0 then anchor ~rate position samples
          else (
            let excess =
              Option.value ~default:samples
                (Time_base.rescale ~src:(Time_base.main ())
                   ~dst:{ Avutil.num = 1; den = rate }
                   (-drift))
            in
            if excess >= samples then None
            else follow (continuity.trim item excess) (samples - excess)))

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

let within_drift t ts =
  match (t.continuity, t.clock, t.origin) with
    | Some continuity, Some clock, Some origin -> (
        match (rescale t ts, rescale t origin) with
          | Some converted, Some converted_origin ->
              abs (t.start + converted - converted_origin - expected clock)
              <= drift_bound t continuity
          | _ -> false)
    | _ -> false

(* The item at position [p] joins the group, or closes it. *)
let accept t ~origin ~ts ~converted ~converted_origin ~position ~p item =
  let closes = match t.group_pos with Some g -> p > g | None -> false in
  if (not closes) && t.group_size >= max_group_items then group_full t
  else (
    let item_end =
      match Option.map (fun c -> c.samples item) t.continuity with
        | Some (Some (samples, rate)) -> Some (p + samples_length ~rate samples)
        | _ -> item_end t ~ts ~converted_origin item
    in
    t.origin <- Some origin;
    (* A placed item's timestamp follows its placement. *)
    let shift = p - position in
    let written =
      if t.convert_ts then converted + shift
      else
        ts
        + Option.value ~default:0
            (Time_base.rescale ~src:t.dst ~dst:t.src shift)
    in
    t.accessors.set_ts item (Some (Int64.of_int written));
    t.last_ts <-
      Some (match t.last_ts with Some last -> max last ts | None -> ts);
    Faults.delivered t.faults;
    let result =
      match t.group_pos with
        | Some group_pos when closes -> `Chunk (emit t ~length:(p - group_pos))
        | _ -> `Nothing
    in
    t.group_pos <- Some p;
    t.group_end <-
      (match (t.group_end, item_end) with
        | Some e, Some e' -> Some (max e e')
        | e, None | None, e -> e);
    append t item;
    result)

let push_timestamped t item ts =
  let ts = ts + t.offset in
  match t.last_ts with
    | Some last when ts < last && not (within_drift t ts) ->
        drop t
          (Printf.sprintf "Decreasing timestamp %d after %d (time base %s)" ts
             last
             (Time_base.to_string t.src))
    | _ -> (
        let origin = Option.value ~default:ts t.origin in
        match (rescale t ts, rescale t origin) with
          | Some converted, Some converted_origin -> (
              let position = t.start + converted - converted_origin in
              let placed =
                match t.continuity with
                  | Some continuity -> place t ~continuity ~position item
                  | None -> Some (position, item)
              in
              match placed with
                | None -> `Nothing
                | Some (p, item) ->
                    accept t ~origin ~ts ~converted ~converted_origin ~position
                      ~p item)
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
        (match (t.continuity, t.clock) with
          | Some continuity, Some clock -> (
              match continuity.samples item with
                | Some (samples, rate) when rate = clock.rate ->
                    t.clock <- Some { clock with fed = clock.fed + samples }
                | _ -> ())
          | _ -> ());
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
