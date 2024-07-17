(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

let rec may_map f = function
  | h::t ->
      (
        match f h with
          | Some h -> h::(may_map f t)
          | None -> may_map f t
      )
  | [] -> []

let really_read fd buf ofs len =
  let l = ref 0 in
  let r = ref (-1) in
    while !l < len && !r <> 0 do
      r := Unix.read fd buf !l (len - !l);
      l := !l + !r
    done;
    !l

(** Perfect Fisher-Yates shuffle
  * (http://www.nist.gov/dads/HTML/fisherYatesShuffle.html). *)
let randomize a =
  let permute i j =
    let tmp = a.(i) in
      a.(i) <- a.(j);
      a.(j) <- tmp
  in
  let l = Array.length a in
    if l>=2 then
      for i=0 to l-2 do
        permute i (i + Random.int (l-i))
      done


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
      "w", string_of_int (t.Unix.tm_wday)            ;
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

(** Decode Base64-encoded data *)
let decode64 s =
  let padding = ref 0 in
  let to_int c = match c with
    | 'A'..'Z' -> int_of_char c - int_of_char 'A'
    | 'a'..'z' -> int_of_char c - int_of_char 'a' + 26
    | '0'..'9' -> int_of_char c - int_of_char '0' + 52
    | '+' -> 62
    | '/' -> 63
    | '=' -> incr padding ; 0
    | _ -> failwith "decode64: invalid encoding"
  in
  let result = ref [] in
  let add x = result := (char_of_int x) :: !result in
    for i = 0 to String.length s / 4 - 1 do
      (* Read 4 64-digits, i.e. 3 bytes. *)
      let c n = to_int s.[i*4+n] in
      let i = (c 3) + (c 2) lsl 6 + (c 1) lsl 12 + (c 0) lsl 18 in
        add ((i land 0xff0000) lsr 16) ;
        add ((i land 0x00ff00) lsr 8) ;
        add ( i land 0x0000ff)
    done ;
    let result =
      (* Remove up to two bytes depending on the padding. *)
      match !padding with
        | 0 -> !result
        | 1 -> List.tl !result
        | 2 -> List.tl (List.tl !result)
        | _ -> failwith "decode64: invalid encoding"
    in
    let len = List.length result in
    let s = String.make len ' ' in
      ignore (List.fold_left (fun i c -> s.[i] <- c ; i-1) (len-1) result) ;
      s

(** Base 64 encoding. *)
let encode64 s =
  let digit =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let extra = String.length s mod 3 in
  let s = match extra with 1 -> s ^ "\000\000" | 2 -> s ^ "\000" | _ -> s in
  let n = String.length s in
  let dst = String.create (4 * (n/3)) in
    for i = 0 to n/3 - 1 do
      let (:=) j v = dst.[i*4+j] <- digit.[v] in
      let c j = int_of_char s.[i*3+j] in
      let c0 = c 0 and c1 = c 1 and c2 = c 2 in
        0 := c0 lsr 2 ;
        1 := ((c0 lsl 4) land 63) lor (c1 lsr 4) ;
        2 := ((c1 lsl 2) land 63) lor (c2 lsr 6) ;
        3 := c2 land 63
    done ;
    if extra = 1 then begin
      dst.[4*(n/3)-2] <- '=' ;
      dst.[4*(n/3)-1] <- '='
    end else if extra = 2 then
      dst.[4*(n/3)-1] <- '=' ;
    dst
