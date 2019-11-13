(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Function call information. *)
type t = {
  total_time: float;  (** Time spent in the function. *)
  self_time: float;  (** Time spent in the function excluding children. *)
}

let calls = ref []

(* Call stack. *)
let stack = ref []

(* Time spent in children. *)
let children = ref [ref 0.]

(** Indicate the time spent in a given function. *)
let add f t = calls := (f, t) :: !calls

(** Measure time for a given function. *)
let time fname f x =
  stack := fname :: !stack ;
  children := ref 0. :: !children ;
  let t0 = Unix.gettimeofday () in
  let ans = f x in
  let t1 = Unix.gettimeofday () in
  stack := List.tl !stack ;
  let children_time = !(List.hd !children) in
  children := List.tl !children ;
  let dt = t1 -. t0 in
  List.hd !children := !(List.hd !children) +. dt ;
  (* TODO: time is counted multiple times in recursive calls. *)
  let total_time = dt in
  let self_time = dt -. children_time in
  let t = {total_time; self_time} in
  add fname t ; ans

module M = Map.Make (struct
  type t = string

  let compare = compare
end)

let stats () =
  let m = ref M.empty in
  List.iter
    (fun (f, t) ->
      m :=
        M.update f (function Some l -> Some (t :: l) | None -> Some [t]) !m)
    !calls ;
  let m = !m in
  let l = M.bindings m in
  let l =
    List.map
      (fun (f, t) ->
        ( f,
          ( List.fold_left (fun x t -> x +. t.self_time) 0. t,
            List.fold_left (fun x t -> x +. t.total_time) 0. t,
            List.length t ) ))
      l
  in
  let l = List.sort (fun (_, t) (_, t') -> -compare t t') l in
  let l =
    List.map
      (fun (f, (self, total, n)) ->
        [|f; string_of_float self; string_of_float total; string_of_int n|])
      l
  in
  let l = [|"function"; "self"; "total"; "calls"|] :: [||] :: l in
  let l = Array.of_list l in
  Utils.string_of_matrix l
