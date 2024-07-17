(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(* Several list utilities *)

let rec make_list n v = if n = 0 then [] else v::(make_list (n-1) v)

let rec prefix p l =
  match p,l with
    | [],_ -> true
    | _,[] -> false
    | hp::tp,hl::tl -> hp=hl && prefix tp tl

let hashtbl_of_list l =
  let h = Hashtbl.create (List.length l) in
    List.iter (fun (k,v) -> Hashtbl.add h k v) l ;
    h

let filter_exists f l =
  let rec aux acc = function
    | [] -> false, List.rev acc
    | x::l -> if f x then true, List.rev_append acc l else aux (x::acc) l
  in aux [] l

(* Here we take care not to introduce new redexes when substituting *)

(* Interpolation:
 * takes a (string*string) Hashtbl.t and
 * a string containing special patterns like $(v) or $(if $(v),"bla","bli")
 * and interpolates them just like make does, using the hash table for
 * variable definitions. *)
let interpolate =
  let quoted = "\"\\(\\([^\"\\]\\|\\(\\\\\"\\)\\)*\\)\"" in (* 3 groups *)
  let var    = "\\$(\\([^()$]+\\))" in
  let re_var = Str.regexp var in
  let re_if  =
    (* Groups              1           2 (3 4)   5       6 (7 8)      *)
    Str.regexp ("\\$(if +"^var^" *, *"^quoted^"\\( *, *"^quoted^"\\)?)")
  in
  let unescape = Str.global_replace (Str.regexp "\\\\\\(.\\)") "\\1" in
    fun m s ->
      let find v = try Hashtbl.find m v with Not_found -> "" in
      let process_if s =
        let s = ref s in
        let changed = ref true in
          while !changed do
            changed := false ;
            s := Str.substitute_first re_if
                   (fun s ->
                      changed := true ;
                      let v = find (Str.matched_group 1 s) in
                      let then_ = Str.matched_group 2 s in
                      let else_ = try Str.matched_group 6 s with _ -> "" in
                        unescape (if v="" then else_ else then_))
                   !s
          done ;
          !s
      in
      let interpolate =
        Str.global_substitute re_var (fun s -> find (Str.matched_group 1 s))
      in
        interpolate (process_if s)
