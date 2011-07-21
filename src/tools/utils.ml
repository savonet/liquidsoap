(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

(* Force locale to C *)
external force_locale : unit -> unit = "liquidsoap_set_locale"

let () = 
  force_locale ()

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

let list_of_metadata m =
  let f x y l = (x,y)::l in
  Hashtbl.fold f m []

let hashtbl_get : ('a,'b) Hashtbl.t -> 'a -> 'b option =
  fun h k ->
    try Some (Hashtbl.find h k) with Not_found -> None

(** Remove the first element satisfying a predicate, raising Not_found
  * if none is found. *)
let remove_one f l =
  let rec aux acc = function
    | [] -> raise Not_found
    | x::l ->
        if f x then List.rev_append acc l else aux (x::acc) l
  in
    aux [] l

let rec may_map f = function
  | h::t ->
      begin match f h with
        | Some h -> h::(may_map f t)
        | None -> may_map f t
      end
  | [] -> []

let really_read fd buf ofs len =
  let l = ref 0 in
  let r = ref (-1) in
    while !l < len && !r <> 0 do
      r := Unix.read fd buf !l (len - !l);
      l := !l + !r
    done;
    !l

(* Read all data from a given filename.
 * We cannot use really_input with the 
 * reported length of the file because
 * some OSes such as windows may do implicit
 * conversions (file opened in text mode in 
 * win32), thus making the actual number of 
 * characters that can be read from the file
 * different than its reported length.. *)
let read_all filename =
  let channel = open_in filename in
  let buflen = 1024 in
  let tmp = String.create 1024 in
  let contents = Buffer.create buflen in
  let rec read () =
    let ret = input channel tmp 0 1024 in
    if ret > 0 then
     begin
      Buffer.add_substring contents tmp 0 ret ;
      read ()
     end
  in
  read () ;
  close_in channel ;
  Buffer.contents contents

(* Drop all but then [len] last bytes. *)
let buffer_drop buffer len =
  let size = Buffer.length buffer in
  assert (len <= size) ;
  if len = size then Buffer.reset buffer else
    let tmp = Buffer.sub buffer len (size-len) in
      Buffer.reset buffer ;
      Buffer.add_string buffer tmp

(* Exception translation and backtrace printing.
 * We provide first a default implementation
 * and override it with Printexc's implementation
 * if present.. *)

(* Exception translation *)
exception Translation of string

let error_translators = Queue.create ()

let register_error_translator x = Queue.push x error_translators

let unix_translator = 
  function
    | Unix.Unix_error (code,name,param) ->
      raise (Translation 
          (Printf.sprintf "%s in %s(%s)" (Unix.error_message code) 
                                         name param))
    | _ -> ()

let () = register_error_translator unix_translator

let exception_printer e =
 try
   Queue.iter (fun f -> f e) error_translators ;
   None
 with
   | Translation x -> Some x

let register_printer _ = 
    raise Not_found

(* Exception backtrace printing.
 * This is used in Threads where 
 * the backtrace seems to be lost
 * otherwise. *)

(* Default implementation when Printexc does
 * not implement it. *)
let get_backtrace () = 
  "Liquidsoap not compiled with ocaml >= 3.11, \
   cannot print stack backtrace"

(* Open Printexc, which overrides register_printer 
 * and get_backtrace. *)
open Printexc

let get_backtrace = get_backtrace

let printexc_has_register = 
  try
    register_printer exception_printer ;
    true
  with
    | Not_found -> false

let error_message e = 
  if printexc_has_register then
    Printexc.to_string e
  else
    match exception_printer e with
      | Some s -> s
      | None   -> Printexc.to_string e

(* Dynamic loading. *)

let dyn_log = Dtools.Log.make ["dynamic";"loader"]

let dynlink_suffix =
  if Dynlink.is_native then
    ".cmxs"
  else
    ".cma"

let load_plugins_dir d =
  (* We need to allow unsafe modules
   * for plugins to work in liquidsoap.
   * Otherwise, plugins that load C stubs
   * will not be available.. Additionaly,
   * this function does nothing in native mode.. *)
  Dynlink.allow_unsafe_modules true;
  try
    let dir = Unix.opendir d in
    let rec files cur =
      try
        let f = Unix.readdir dir in
        let f = Printf.sprintf "%s/%s" d f in
        if Filename.check_suffix f dynlink_suffix then
           files (f :: cur)
        else
           files cur
      with
        | End_of_file -> cur
    in
    let files = files [] in
    Unix.closedir dir;
    (* Brute force dependency method:
     * Try to load plugins in any order
     * and stop when the list does not shrink.. *)
    let load ~report cur file =
     try
       Dynlink.loadfile file;
       dyn_log#f 2 "Loaded plugin file %s." file;
       cur
      with
        | Dynlink.Error e when report ->
            dyn_log#f 2 "Could not load plugin file %s: %s."
             file (Dynlink.error_message e);
            cur
        | e ->
             if report then
              begin
                dyn_log#f 2 "Unknown error while loading plugin file %s: %s"
                  file (error_message e) ;
                cur
              end
             else
              file :: cur
    in
    let rec try_load files =
      let new_files =
        List.fold_left (load ~report:false) [] files
      in
      if List.length new_files = List.length files then
        (* Run a last time to report errors.. *)
        ignore(List.fold_left (load ~report:true) [] files)
      else
        (* List has shrinked, run again.. *)
        try_load new_files
    in
    try_load files
  with
    | _ -> dyn_log#f 2 "Could not load plugins in directory %s." d

type dynload =
  { path : string list;
    (* Files are registered *without*
     * their extension. e.g.: foo.cmxs -> foo *)
    files : string list;
    load : unit -> unit }

let dynlink_list = Hashtbl.create 2

exception Done of string

(* A function to load external libraries (currently
 * lame and aacplus) *)
let load_dynlinks () =
  let rec check_list f cur =
    match cur with
      | x :: l -> 
         if f x then
           check_list f l
         else false
      | [] -> true
  in
  let load_library name dynload =
    try
     List.iter (fun path ->
      try
       let get_file = 
         (fun f -> Printf.sprintf "%s/%s%s" path f dynlink_suffix)
       in
       if check_list
           (fun file -> Sys.file_exists (get_file file))
           dynload.files then
        begin
         List.iter (fun file ->
                       Dynlink.loadfile (get_file file))
                   dynload.files;
         raise (Done path)
        end
      with
        | Dynlink.Error e ->
            dyn_log#f 3 "Error while loading dynamic %s at %s" name path;
            dyn_log#f 4 "%s" (Dynlink.error_message e)) dynload.path;
     dyn_log#f 3 "Could not find dynamic module for %s." name
    with
      | Done path ->
          dyn_log#f 3 "Loaded dynamic %s from %s" name path;
          dynload.load ()
  in
  Hashtbl.iter load_library dynlink_list

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

let array_iter2 a b f =
  assert (Array.length a = Array.length b) ;
  for i = 0 to Array.length a - 1 do
    f a.(i) b.(i)
  done

let special_char = 
  function
    | '"' 
    | '\\'
    (* DEL *)
    | '\x7F'
    (* Control chars *)
    | '\x00'..'\x1F' -> true
    | _ -> false

let escape_char c = 
  if c <> '"' then 
    Char.escaped c
  else
    "\\\""

(* Generic escaping function *)
let escape ?(special_char=special_char) 
           ?(next=(fun _ i -> i+1)) 
           ?(escape_char=escape_char)
           f s =
  let out = Format.pp_print_char f in
  let outs = Format.pp_print_string f in
  let len = String.length s in
  let rec f pos = 
    let c = String.unsafe_get s pos in
    if special_char c then
      outs (escape_char c)
    else
      out c ;
    let new_pos = next s pos in
    for i = pos+1 to (min (new_pos -1)
                          (len - 1)) 
    do
      out (String.unsafe_get s i)
    done ;
    if new_pos < len then
      f new_pos
  in
  out '"' ;
  if len > 0 then
    f 0 ;
  out '"'

(* These two functions are taken from Extlib's module UTF8
 * Copyright (c) 2002, 2003 Yamagata Yoriyuki *)
let rec utf8_search_head s i =
  if i >= String.length s then i else
  let n = Char.code (String.unsafe_get s i) in
  if n < 0x80 || n >= 0xc2 then i else
  utf8_search_head s (i + 1)
let utf8_next s i =
  let n = Char.code s.[i] in
  if n < 0x80 then i + 1 else
  if n < 0xc0 then utf8_search_head s (i + 1) else
  if n <= 0xdf then i + 2
  else if n <= 0xef then i + 3
  else if n <= 0xf7 then i + 4
  else if n <= 0xfb then i + 5
  else if n <= 0xfd then i + 6
  else failwith "Utils.utf_8.next"
(* End of Extlib code *)

let escape_utf8_char =
  function
      | '"'    -> "\\\""
      | '\t'   -> "\\t"
      | '\r'   -> "\\r"
      | '\b'   -> "\\b"
      | '\n'   -> "\\n"
      | '\012' -> "\\f"
      | '\\'   -> "\\\\"
      | '/'    -> "\\/"
      | c ->
          Printf.sprintf "\\u%04X" (int_of_char c)

let escape_utf8 ?special_char ?(escape_char=escape_utf8_char) = 
  escape ?special_char ~escape_char ~next:utf8_next

let escape_string escape s = 
  let b = Buffer.create (String.length s) in
  let f = Format.formatter_of_buffer b in
  escape f s ;
  Format.pp_print_flush f () ;
  Buffer.contents b

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
  try Unix.select x y z t with
    | Unix.Unix_error (Unix.EINTR,_,_) -> select x y z t

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

(** Get a file/uri extension. *)
(* This is not garanteed to work 100% but should
 * be ok on reasonable cases. A problematic cases
 * is for instance: http://bla.com/foo.mp3?gni=bla.truc *)
let get_ext s = 
 try
  let rex = Pcre.regexp "\\.([a-zA-Z0-9]+)[^.]*$" in
  let ret = Pcre.exec ~rex s in
  String.lowercase (Pcre.get_substring ret 1)
 with
   | _ -> raise Not_found

let name_of_sockaddr ?(rev_dns=true) ?(show_port=false) a =
  match a with
    | Unix.ADDR_UNIX s -> Printf.sprintf "unix socket %s" s
    | Unix.ADDR_INET (x,p) ->
         let host = 
            try
              if not rev_dns then raise Not_found ;
              (Unix.gethostbyaddr x).Unix.h_name
            with
              | Not_found -> Unix.string_of_inet_addr x
        in
        if show_port then
          Printf.sprintf "%s:%i" host p
        else
          host

(** Uptime since the application was started. *)
let uptime =
  let base = Unix.gettimeofday () in
    fun () -> Unix.gettimeofday () -. base
