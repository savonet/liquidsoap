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

(** Reduce time predicates, written in scripts as [{9h-21h}] or given to
    [time.predicate] as a string. *)

open Parsed_term
open Term_reducer_helpers

(** The builtins a reduced time predicate calls. *)
type builtin = [ `Time_in_mod | `Not | `And | `Or ] [@@deriving enumerate]

let name : builtin -> string = function
  | `Time_in_mod -> "time_in_mod"
  | `Not -> "not"
  | `And -> "and"
  | `Or -> "or"

let builtins = List.map name all_of_builtin
let builtin ~pos b = mk ~pos (`Var (name b))
let time_units = [| 7 * 24 * 60 * 60; 24 * 60 * 60; 60 * 60; 60; 1 |]

(** Given a date specified as a list of four values (whms), return a date in
    seconds from the beginning of the week. *)
let date ~pos =
  let to_int = function None -> 0 | Some i -> i in
  let rec aux = function
    | None :: tl -> aux tl
    | [] -> parse_error ~pos "Invalid time."
    | l ->
        let a = Array.of_list l in
        let n = Array.length a in
        let tu = time_units and tn = Array.length time_units in
        Array.fold_left ( + ) 0
          (Array.mapi
             (fun i s ->
               let s = if n = 4 && i = 0 then to_int s mod 7 else to_int s in
               tu.(tn - 1 + i - n + 1) * s)
             a)
  in
  aux

(** Give the index of the first non-None value in the list. *)
let last_index l =
  let rec last_index n = function
    | x :: tl -> if x = None then last_index (n + 1) tl else n
    | [] -> n
  in
  last_index 0 l

(** Give the precision of a date-as-list. For example, the precision of Xs is 1,
    XmYs is 60, XhYmZs 3600, etc. *)
let precision d = time_units.(last_index d)

(** Give the duration of a data-as-list. For example, the duration of Xs is 1,
    Xm 60, XhYm 60, etc. *)
let duration d =
  time_units.(Array.length time_units - 1 - last_index (List.rev d))

let between ~pos d1 d2 =
  let d1 = [d1.week; d1.hours; d1.minutes; d1.seconds] in
  let d2 = [d2.week; d2.hours; d2.minutes; d2.seconds] in
  let p1 = precision d1 in
  let p2 = precision d2 in
  let t1 = date ~pos d1 in
  let t2 = date ~pos d2 in
  if p1 <> p2 then parse_error ~pos "Invalid time interval: precisions differ.";
  (t1, t2, p1)

let during ~pos d =
  let d = [d.week; d.hours; d.minutes; d.seconds] in
  let t, d, p = (date ~pos d, duration d, precision d) in
  (t, t + d, p)

let mk_time_pred ~pos (a, b, c) =
  let args = List.map (fun x -> ("", mk_implicit ~pos (`Int x))) [a; b; c] in
  `App (mk_implicit ~pos (`Var (name `Time_in_mod)), args)

let rec to_term { time_pos = pos; time_descr } =
  match time_descr with
    | `Time t -> mk ~pos (mk_time_pred ~pos (during ~pos t))
    | `Time_interval (t, t') -> mk ~pos (mk_time_pred ~pos (between ~pos t t'))
    | `Not p -> mk ~pos (`App (builtin ~pos `Not, [("", to_term p)]))
    | `And (p, p') -> bool_op ~pos `And p p'
    | `Or (p, p') -> bool_op ~pos `Or p p'

(* [and] and [or] take getters, to evaluate their right operand lazily. *)
and bool_op ~pos op p p' =
  let getter p = ("", mk_implicit_fun ~pos [] (to_term p)) in
  mk ~pos (`App (builtin ~pos op, [getter p; getter p']))
