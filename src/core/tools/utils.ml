(**********************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

include Liquidsoap_lang.Utils

let select = if Sys.win32 then Unix.select else Duppy.poll

(* Util to log exception and backtrace together
   when log level is set to info and just exception
   as severe otherwise. Backtrace should be captured as early
   as possible. *)
let log_exception ~(log : Log.t) ~bt msg =
  log#severe "%s" msg;
  if log#active 4 (* info *) then log#info "%s" bt

module Thread = struct
  external set_current_thread_name : string -> unit
    = "liquidsoap_set_current_thread_name"

  let set_current_thread_name s =
    let s =
      if Liquidsoap_lang.Build_config.system = "linux" then (
        let s =
          Re.Pcre.substitute
            ~rex:(Re.Pcre.regexp "[\\s]+[aeiou]")
            ~subst:(fun s -> String.capitalize_ascii (String.trim s))
            s
        in
        let s =
          Re.Pcre.substitute
            ~rex:(Re.Pcre.regexp "[aeiouy\\s]+")
            ~subst:(fun _ -> "")
            s
        in
        let s =
          Re.Pcre.substitute
            ~rex:(Re.Pcre.regexp "[^a-zA-Z0-9-_]+")
            ~subst:(fun _ -> ":")
            s
        in
        let s =
          List.fold_left
            (fun s c ->
              Re.Pcre.substitute
                ~rex:(Re.Pcre.regexp ("[" ^ c ^ "]+"))
                ~subst:(fun _ -> c)
                s)
            s
            [
              "b";
              "c";
              "d";
              "f";
              "g";
              "h";
              "i";
              "j";
              "k";
              "l";
              "m";
              "n";
              "p";
              "q";
              "r";
              "s";
              "t";
              "v";
              "w";
              "x";
              "z";
            ]
        in
        "LQ:" ^ s)
      else s
    in
    set_current_thread_name s

  include Thread
end

(* Force locale *)
external force_locale : string -> unit = "liquidsoap_set_locale"

(** Get page size. *)
external pagesize : unit -> int = "liquidsoap_get_pagesize"
[@@noalloc]

let pagesize = pagesize ()
let () = force_locale "C"

(* Several list utilities *)

let rec prefix p l =
  match (p, l) with
    | [], _ -> true
    | _, [] -> false
    | hp :: tp, hl :: tl -> hp = hl && prefix tp tl

(** Remove the first element satisfying a predicate, raising Not_found if none
    is found. *)
let remove_one f l =
  let rec aux acc = function
    | [] -> raise Not_found
    | x :: l -> if f x then List.rev_append acc l else aux (x :: acc) l
  in
  aux [] l

(** Read all data from a given filename. We cannot use really_input with the
    reported length of the file because some OSes such as windows may do
    implicit conversions (file opened in text mode in win32), thus making the
    actual number of characters that can be read from the file different than
    its reported length.. *)
let read_all filename =
  let channel = open_in filename in
  let tmp = Bytes.create pagesize in
  let contents = Strings.Mutable.empty () in
  let rec read () =
    let ret = input channel tmp 0 pagesize in
    if ret > 0 then (
      Strings.Mutable.add_subbytes contents tmp 0 ret;
      read ())
  in
  read ();
  close_in channel;
  Strings.Mutable.to_string contents

let copy ?(mode = [Open_wronly; Open_creat; Open_trunc]) ?(perms = 0o660) src
    dst =
  let oc = open_out_gen mode perms dst in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      set_binary_mode_out oc true;
      let ic = open_in_bin src in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = 4096 in
          let buf = Bytes.create len in
          let rec f () =
            match input ic buf 0 len with
              | 0 -> ()
              | n ->
                  output_substring oc (Bytes.unsafe_to_string buf) 0 n;
                  f ()
          in
          f ()))

(* Drop the first [len] bytes. *)
let buffer_drop buffer len =
  let size = Buffer.length buffer in
  assert (len <= size);
  if len = size then Buffer.clear buffer
  else (
    let tmp = Buffer.sub buffer len (size - len) in
    Buffer.clear buffer;
    Buffer.add_string buffer tmp)

let unix_translator = function
  | Unix.Unix_error (code, name, param) ->
      Some (Printf.sprintf "%s in %s(%s)" (Unix.error_message code) name param)
  | _ -> None

let () = Printexc.register_printer unix_translator

(* Here we take care not to introduce new redexes when substituting *)

(* Interpolation:
   takes a (string -> string) lookup function (raise Not_found on failure) and
   a string containing special patterns like $(v) or $(if $(v),"bla","bli")
   and interpolates them just like make does, using the hash table for
   variable definitions. *)
let interpolate =
  let log = Log.make ["string"; "interpolate"] in
  (* TODO Use PCRE *)
  let quoted = "\"\\(\\([^\"\\]\\|\\(\\\\\"\\)\\)*\\)\"" in
  (* 3 groups *)
  let var = "\\$(\\([^()$]+\\))" in
  let re_if =
    (* Groups              1           2 (3 4)   5       6 (7 8)      *)
    (* TODO: use unescape above? *)
    Str.regexp
      ("\\$(if +" ^ var ^ " *, *" ^ quoted ^ "\\( *, *" ^ quoted ^ "\\)?)")
  in
  let unescape = Str.global_replace (Str.regexp "\\\\\\(.\\)") "\\1" in
  fun find s ->
    let find v = try find v with Not_found -> "" in
    let process_if s =
      let s = ref s in
      let changed = ref true in
      while !changed do
        changed := false;
        s :=
          Str.substitute_first re_if
            (fun s ->
              changed := true;
              let v = find (Str.matched_group 1 s) in
              let then_ = Str.matched_group 2 s in
              let else_ = try Str.matched_group 6 s with _ -> "" in
              unescape (if v = "" then else_ else then_))
            !s
      done;
      !s
    in
    let interpolate =
      Str.global_substitute
        (Str.regexp ("\\(.\\|^\\)\\(" ^ var ^ "\\)"))
        (fun s ->
          let p = Str.matched_group 1 s in
          if p = "\\" then Str.matched_group 2 s
          else p ^ find (Str.matched_group 3 s))
    in
    try interpolate (process_if s)
    with exn ->
      let bt = Printexc.get_backtrace () in
      log_exception ~log ~bt
        (Printf.sprintf "Error while interpolating string: %s"
           (Printexc.to_string exn));
      s

(** [which s] is equivalent to /usr/bin/which s, raises Not_found on error *)
let which ~path s =
  let test fname =
    try
      Unix.access fname [Unix.X_OK];
      true
    with _ -> false
  in
  if s = "" then raise Not_found;
  if test s then s
  else List.find test (List.map (fun d -> Filename.concat d s) path)

let which_opt ~path s = try Some (which ~path s) with Not_found -> None

(** Get current timezone. *)
external timezone : unit -> int = "liquidsoap_get_timezone"
[@@noalloc]

external timezone_by_name : unit -> string * string
  = "liquidsoap_get_timezone_by_name"

(* Same as [Unix.mktime] but honnors [isdst] *)
type tm = {
  tm_sec : int;
  tm_min : int;
  tm_hour : int;
  tm_mday : int;
  tm_mon : int;
  tm_year : int;
  tm_isdst : bool option;
}

external mktime : tm -> float = "liquidsoap_mktime"

let string_of_timezone tz =
  (* TODO: not sure about why we need this... *)
  let tz = -tz in
  Printf.sprintf "%+03d%02d" (tz / 3600) (abs (tz / 60) mod 60)

(** Very partial strftime clone *)
let strftime ?time str : string =
  let time =
    match time with Some time -> time | None -> Unix.gettimeofday ()
  in
  let t = Unix.localtime time in
  let weekday =
    [|
      "Sunday";
      "Monday";
      "Tuesday";
      "Wednesday";
      "Thursday";
      "Friday";
      "Saturday";
    |]
  in
  let weekday_abbr = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let month =
    [|
      "January";
      "Febrary";
      "March";
      "April";
      "May";
      "June";
      "July";
      "August";
      "September";
      "October";
      "November";
      "December";
    |]
  in
  let month_abbr =
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |]
  in
  let assoc =
    [
      ("a", weekday_abbr.(t.Unix.tm_wday));
      ("A", weekday.(t.Unix.tm_wday));
      ("b", month_abbr.(t.Unix.tm_mon));
      ("B", month.(t.Unix.tm_mon));
      ("S", Printf.sprintf "%02d" t.Unix.tm_sec);
      ("M", Printf.sprintf "%02d" t.Unix.tm_min);
      ("H", Printf.sprintf "%02d" t.Unix.tm_hour);
      ("d", Printf.sprintf "%02d" t.Unix.tm_mday);
      ("j", Printf.sprintf "%03d" t.Unix.tm_yday);
      ("m", Printf.sprintf "%02d" (t.Unix.tm_mon + 1));
      ("p", if t.Unix.tm_hour < 12 then "AM" else "PM");
      ("Y", string_of_int (t.Unix.tm_year + 1900));
      ("w", string_of_int t.Unix.tm_wday);
      ("z", string_of_timezone (timezone ()));
      ("%", "%");
    ]
  in
  let subst sub =
    let key = Re.Pcre.get_substring sub 1 in
    try List.assoc key assoc with _ -> "%" ^ key
  in
  Re.replace (Re.Pcre.regexp "%(.)") ~f:subst str

(** Check if a directory exists. *)
let is_dir d =
  try
    ignore (Sys.readdir d);
    true
  with _ -> false

let dir_exists d = Sys.file_exists d && is_dir d

(** Create a directory, and its parents if needed. Raise Unix_error on error. *)
let rec mkdir ~perm dir =
  if Sys.file_exists dir then
    if is_dir dir then ()
    else raise (Unix.Unix_error (Unix.ENOTDIR, "Utils.mkdir", dir))
  else (
    let up = Filename.dirname dir in
    if up = "." then () else mkdir ~perm up;
    Unix.mkdir dir perm)

let ensure_dir ~perm filename =
  let dir = Filename.dirname filename in
  mkdir ~perm dir

let get_tempdir () =
  if Sys.win32 then Option.value (Sys.getenv_opt "TEMP") ~default:"C:\\temp"
  else Option.value (Sys.getenv_opt "TMPDIR") ~default:"/tmp"

(* This is not guaranteed to work 100% but should
   be ok on reasonable cases. A problematic cases
   is for instance: http://bla.com/foo.mp3?gni=bla.truc *)

(** Get a file/uri extension. *)
let get_ext s =
  try
    let rex = Re.Pcre.regexp "\\.([a-zA-Z0-9]+)[^.]*$" in
    let ret = Re.Pcre.exec ~rex s in
    String.lowercase_ascii (Re.Pcre.get_substring ret 1)
  with _ -> raise Not_found

let get_ext_opt s = try Some (get_ext s) with Not_found -> None

let name_of_sockaddr ?(rev_dns = false) ?(show_port = false) a =
  match a with
    | Unix.ADDR_UNIX s -> Printf.sprintf "unix socket %S" s
    | Unix.ADDR_INET (x, p) ->
        let host =
          try
            if not rev_dns then raise Not_found;
            (Unix.gethostbyaddr x).Unix.h_name
          with Not_found -> Unix.string_of_inet_addr x
        in
        if show_port then Printf.sprintf "%s:%i" host p else host

(** Uptime since the application was started. *)
let uptime =
  let base = Unix.gettimeofday () in
  fun () -> Unix.gettimeofday () -. base

(** Generate a string which can be used as a parameter name. *)
let normalize_parameter_string s =
  let s =
    Re.Pcre.substitute
      ~rex:(Re.Pcre.regexp "( *\\([^\\)]*\\)| *\\[[^\\]]*\\])")
      ~subst:(fun _ -> "")
      s
  in
  let s =
    Re.Pcre.substitute
      ~rex:(Re.Pcre.regexp "(\\.+|\\++)")
      ~subst:(fun _ -> "")
      s
  in
  let s =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp " +$") ~subst:(fun _ -> "") s
  in
  let s =
    Re.Pcre.substitute
      ~rex:(Re.Pcre.regexp "( +|/+|-+)")
      ~subst:(fun _ -> "_")
      s
  in
  let s =
    Re.Pcre.substitute ~rex:(Re.Pcre.regexp "\"") ~subst:(fun _ -> "") s
  in
  let s = String.lowercase_ascii s in
  (* Identifiers cannot begin with a digit. *)
  let s =
    if Re.Pcre.pmatch ~rex:(Re.Pcre.regexp "^[0-9]") s then "_" ^ s else s
  in
  s

(** A function to reopen a file descriptor Thanks to Xavier Leroy!

    Ref:
    http://caml.inria.fr/pub/ml-archives/caml-list/2000/01/a7e3bbdfaab33603320d75dbdcd40c37.en.html
*)
let reopen_out outchan filename =
  flush outchan;
  let fd1 = Unix.descr_of_out_channel outchan in
  let fd2 = Unix.openfile filename [Unix.O_WRONLY; Unix.O_CLOEXEC] 0o666 in
  Unix.dup2 ~cloexec:true fd2 fd1;
  Unix.close fd2

(** The same for inchan *)
let reopen_in inchan filename =
  let fd1 = Unix.descr_of_in_channel inchan in
  let fd2 = Unix.openfile filename [Unix.O_RDONLY; Unix.O_CLOEXEC] 0o666 in
  Unix.dup2 ~cloexec:true fd2 fd1;
  Unix.close fd2

(* See: http://www.onicos.com/staff/iz/formats/ieee.c *)
let float_of_extended_float bytes =
  let float_of_unsigned u =
    let ( - ) = Int32.sub in
    let f = Int32.shift_left Int32.one 31 - Int32.one in
    Int32.to_float (u - f - Int32.one) +. 2147483648.
  in
  let expon =
    ((int_of_char bytes.[0] land 0x7F) lsl 8)
    lor (int_of_char bytes.[1] land 0xff)
  in
  let boc c = Int32.of_int (int_of_char c land 0xff) in
  let ( lsl ) = Int32.shift_left in
  let ( lor ) = Int32.logor in
  let hiMant =
    (boc bytes.[2] lsl 24)
    lor (boc bytes.[3] lsl 16)
    lor (boc bytes.[4] lsl 8)
    lor boc bytes.[5]
  in
  let loMant =
    (boc bytes.[6] lsl 24)
    lor (boc bytes.[7] lsl 16)
    lor (boc bytes.[8] lsl 8)
    lor boc bytes.[9]
  in
  if expon = 0 && hiMant = Int32.zero && loMant = Int32.zero then 0.
  else if expon = 0x7fff then nan
  else (
    let expon = expon - 16383 - 31 in
    let f = ldexp (float_of_unsigned hiMant) expon in
    let f = f +. ldexp (float_of_unsigned loMant) (expon - 32) in
    if int_of_char bytes.[0] land 0x80 <> 0 then -1. *. f else f)

(** Size of an OCaml value (including referred elements) in bytes. *)
let reachable_size x = Obj.reachable_words (Obj.repr x) * Sys.word_size

(** String representation of a size (in bytes). *)
let string_of_size n =
  if n < 1 lsl 10 then Printf.sprintf "%d B" n
  else if n < 1 lsl 20 then
    Printf.sprintf "%.02f KiB" (float_of_int n /. float_of_int (1 lsl 10))
  else if n < 1 lsl 30 then
    Printf.sprintf "%.02f MiB" (float_of_int n /. float_of_int (1 lsl 20))
  else Printf.sprintf "%.02f GiB" (float_of_int n /. float_of_int (1 lsl 30))

let var_script = ref "default"

let substs =
  ref
    [
      ("<script>", fun () -> !var_script);
      ("<pid>", fun () -> string_of_int (Unix.getpid ()));
      ("<home>", fun () -> try Sys.getenv "HOME" with Not_found -> "<home>");
    ]

let add_subst r s = substs := (r, fun () -> s) :: !substs

let subst_vars s =
  List.fold_left
    (fun v (r, s) -> Str.global_replace (Str.regexp r) (s ()) v)
    s !substs

let concat_with_last ~last sep l =
  match List.rev l with
    | [] -> ""
    | [x] -> x
    | [x; y] -> Printf.sprintf "%s %s %s" y last x
    | x :: l ->
        Printf.sprintf "%s %s %s" (String.concat sep (List.rev l)) last x

(* Stdlib.abs_float is not inlined!. *)
let abs_float (f : float) = if f < 0. then -.f else f [@@inline always]

let frame_id_of_string = function
  | "comment" -> Some `COMM
  | "album" -> Some `TALB
  | "bpm" -> Some `TBPM
  | "composer" -> Some `TCOM
  | "content" -> Some `TCON
  | "copyright" -> Some `TCOP
  | "date" -> Some `TDAT
  | "encoder" -> Some `TENC
  | "title" -> Some `TIT2
  | "language" -> Some `TLAN
  | "length" -> Some `TLEN
  | "performer" -> Some `TOPE
  | "artist" -> Some `TPE1
  | "band" -> Some `TPE2
  | "publisher" -> Some `TPUB
  | "tracknumber" -> Some `TRCK
  | "year" -> Some `TYER
  | "url" -> Some `WXXX
  | v -> Metadata.ID3v2.frame_id_of_string v

let id3v2_of_metadata ~version m =
  let frames =
    List.fold_left
      (fun frames (k, v) ->
        match frame_id_of_string k with
          | Some id ->
              {
                Metadata.ID3v2.id;
                data =
                  (if Metadata.ID3v2.binary_frame id then `Binary v
                   else `Text (`UTF_8, v));
                flags = Metadata.ID3v2.default_flags id;
              }
              :: frames
          | None -> frames)
      [] m
  in
  Metadata.ID3v2.make ~version frames

let is_docker =
  Lazy.from_fun (fun () ->
      Sys.unix
      && Sys.command "grep 'docker\\|lxc' /proc/1/cgroup >/dev/null 2>&1" = 0)

let optional_apply fn = function None -> () | Some v -> fn v
