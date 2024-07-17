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

let get_some = function Some x -> x | None -> assert false

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

let hashtbl_get : ('a,'b) Hashtbl.t -> 'a -> 'b option =
  fun h k ->
    try Some (Hashtbl.find h k) with Not_found -> None

let filter_exists f l =
  let rec aux acc = function
    | [] -> false, List.rev acc
    | x::l -> if f x then true, List.rev_append acc l else aux (x::acc) l
  in aux [] l

(* Here we take care not to introduce new redexes when substituting *)

(* Interpolation:
 * takes a (string -> string) lookup function (raise Not_found on failure) and
 * a string containing special patterns like $(v) or $(if $(v),"bla","bli")
 * and interpolates them just like make does, using the hash table for
 * variable definitions. *)
let interpolate =
  (* TODO Use PCRE *)
  let quoted = "\"\\(\\([^\"\\]\\|\\(\\\\\"\\)\\)*\\)\"" in (* 3 groups *)
  let var    = "\\$(\\([^()$]+\\))" in
  let re_if  =
    (* Groups              1           2 (3 4)   5       6 (7 8)      *)
    Str.regexp ("\\$(if +"^var^" *, *"^quoted^"\\( *, *"^quoted^"\\)?)")
  in
  let unescape = Str.global_replace (Str.regexp "\\\\\\(.\\)") "\\1" in
    fun find s ->
      let find v = try find v with Not_found -> "" in
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
        Str.global_substitute (Str.regexp ("\\(.\\|^\\)\\("^var^"\\)"))
          (fun s ->
             let p = Str.matched_group 1 s in
               if p="\\" then Str.matched_group 2 s else
                 p^(find (Str.matched_group 3 s)))
      in
        interpolate (process_if s)

(** [which s] is equivalent to /usr/bin/which s, raises Not_found on error *)
let which =
  let path =
    let s = Sys.getenv "PATH" in
      Str.split (Str.regexp_string ":") s
  in
    fun s ->
      if Sys.file_exists s then s else
        List.find Sys.file_exists (List.map (fun d -> d^"/"^s) path)

(** Uninterruptible select *)
let rec select x y z t =
  try Unix.select x y z t with Unix.Unix_error (Unix.EINTR,_,_) -> select x y z t

(** Very partial strftime clone *)
let strftime str : string =
  let t = Unix.localtime (Unix.gettimeofday ()) in
  let assoc =
    [ "S", Printf.sprintf "%02d" t.Unix.tm_sec       ;
      "M", Printf.sprintf "%02d" t.Unix.tm_min       ;
      "H", Printf.sprintf "%02d" t.Unix.tm_hour      ;
      "d", Printf.sprintf "%02d" t.Unix.tm_mday      ;
      "m", Printf.sprintf "%02d" (t.Unix.tm_mon + 1) ;
      "Y", string_of_int (t.Unix.tm_year + 1900)     ;
      "%", "%" ]
  in
  let subst sub =
    let key = Pcre.get_substring sub 1 in
      try List.assoc key assoc with _ -> "%"^key
  in
    Pcre.substitute_substrings ~pat:"%(.)" ~subst str

(** Check if a directory exists. *)
let is_dir d     = try ignore (Sys.readdir d) ; true with _ -> false
let dir_exists d = Sys.file_exists d && is_dir d

(** Create a directory, and its parents if needed.
  * Raise Unix_error on error. *)
let rec mkdir ~perm dir =
  if Sys.file_exists dir then
    if is_dir dir then () else
      raise (Unix.Unix_error (Unix.ENOTDIR,"Utils.mkdir",dir))
  else
    let up = Filename.dirname dir in
      if up = "." then () else mkdir ~perm up ;
      Unix.mkdir dir perm

(** Expand ~ notation in filenames. *)
let home_unrelate =
  let home =
    try Some (Sys.getenv "HOME") with Not_found -> None
  in
  let unrel s =
    let len = String.length s in
      if len < 2 then
        match home with Some h when s = "~" -> h | _ -> s
      else
        match home, (s.[0] = '~'), (s.[1] = '/') with
          | Some home, true, true ->  (* Something like ~/data/file *)
              Filename.concat home (String.sub s 2 (len - 2))
          | _, true, false ->         (* Something like ~bob/data/file *)
              let index =
                try String.index s '/' with Not_found -> len
              in
              let user = String.sub s 1 (index - 1) in
                begin try
                  let home = (Unix.getpwnam user).Unix.pw_dir in
                    Filename.concat home (String.sub s index (len - index))
                with
                  | Not_found -> s
                end
          | _ -> s
  in
    unrel

let get_tempdir () =
  if Sys.os_type = "Win32" then
    (try Sys.getenv "TEMP" with Not_found -> "C:\temp")
  else
    (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
