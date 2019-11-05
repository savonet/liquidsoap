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

let calls = ref ([] : (string * float) list)

(** Indicate the time spent in a given function. *)
let add f t =
  calls := (f,t) :: !calls

(** Measure time for a given function. *)
let time fname f x =
  let t0 = Unix.gettimeofday () in
  let ans = f x in
  let t1 = Unix.gettimeofday () in
  add fname (t1 -. t0);
  ans

module M = Map.Make(struct type t = string let compare = compare end)

let stats () =
  let m = ref M.empty in
  List.iter
    (fun (f,t) ->
       m :=
         M.update f
           (function
             | Some l -> Some (t::l)
             | None -> Some [t]
           ) !m
    ) !calls;
  let m = !m in
  let l = M.bindings m in
  let l = List.map (fun (f,tt) -> f, List.fold_left (+.) 0. tt) l in
  let l = List.sort (fun (_,t) (_,t') -> - compare t t') l in
  "PROFILING STATS\n\n" ^
  String.concat "" (List.map (fun (f,t) -> Printf.sprintf "%s: %f\n" f t) l)
