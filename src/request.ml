(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

(** Plug for resolving, that is obtaining a file from an URI.
  * [src/protocols] plugins provide ways
  * to resolve URIs: fetch, generate, ... *)

open Dtools

let conf =
  Conf.void ~p:(Configure.conf#plug "request") "requests configuration"
let conf_max_rid =
  Conf.int ~p:(conf#plug "max_id") ~d:Configure.requests_max_id
    "maximum number of requests"

let log = Log.make ["request"]

(** File utilities. *)

let remove_file_proto =
  (* TODO Use PCRE *)
  let re = Str.regexp "^file://" in
    fun s ->
      Str.replace_first re "" s

let home_unrelate s = Utils.home_unrelate (remove_file_proto s)

let parse_uri uri =
  try
    let i = String.index uri ':' in
      (String.sub uri 0 i),
      (String.sub uri (i+1) ((String.length uri)-(i+1)))
  with
    | _ -> "",""

let cleanup =
  let re1 = Str.regexp "^[\t ]*" in
  let re2 = Str.regexp "[\t ]*$" in
    fun s -> Str.global_replace re1 "" (Str.global_replace re2 "" s)

let get_extension f =
  try
    let i = 1 + String.rindex f '.' in
    let len = String.length f in
      if i >= len then "" else
	String.sub f i (len-i)
  with
    | Not_found -> ""

(** Metadata *)

type metadata = (string,string) Hashtbl.t

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
  else failwith "Request.utf_8.next"
(* End of Extlib code *)

(* The utf8_S_formatter provides some of the features of the standard S
 * formatter, but supports UTF8 encoded strings. So it won't escape wide
 * characters and mistake continuations of characters with normal ones.
 * Currently, it escapes '\n' and '"'. *)
let utf8_S_formatter f s =
  let i = ref 0 in
  let next = utf8_next s in
  let l = String.length s in
  let out = Format.pp_print_char f in
  let outs = Format.pp_print_string f in
  let out_sub i j =
    for n = i to j do out s.[n] done
  in
    out '"' ;
    while !i < l do
      let n = next !i in
      if n-1 < l then
        if n > !i+1 then out_sub !i (n-1) else
          if s.[!i] = '"' then outs "\\\"" else
          if s.[!i] = '\n' then outs "\\n" else
            out s.[!i] ;
      i := n
    done ;
    out '"'

let string_of_metadata metadata =
  let b = Buffer.create 20 in
  let f = Format.formatter_of_buffer b in
  let first = ref true in
    Hashtbl.iter (fun k v ->
                    if !first then begin
                      first := false ;
                      Format.fprintf f "%s=%a" k utf8_S_formatter v
                    end else
                      Format.fprintf f "\n%s=%a" k utf8_S_formatter v)
      metadata ;
    Format.pp_print_flush f () ;
    Buffer.contents b

let short_string_of_metadata m =
  "Title: "^(
    try
      let t = Hashtbl.find m "title" in
	if String.length t < 12 then t else
	  (String.sub t 0 9)^"..."
    with
      | Not_found -> "(undef)"
  )

(** Log *)

type log = (Unix.tm * string) Queue.t

let pretty_date date =
  Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d"
    (date.Unix.tm_year+1900)
    (date.Unix.tm_mon+1) date.Unix.tm_mday
    date.Unix.tm_hour date.Unix.tm_min date.Unix.tm_sec

let string_of_log log =
  Queue.fold (fun s (date,msg) ->
                s^(if s="" then
                     Printf.sprintf "[%s] %s" (pretty_date date) msg
                   else
                     Printf.sprintf "\n[%s] %s" (pretty_date date) msg)) "" log

(** Requests.
  * The purpose of a request is to get a valid file. The file can be audio,
  * in which case validity implies finding a working decoder, or can be
  * something arbitrary, like a playlist.
  * This file is fetched using protocols. For example the fetching can involve
  * querying a mysql database, receiving a list of new URIS, using http to
  * download the first URI, check it, fail, using smb to download the second,
  * success, have the file played, finish the request, erase the temporary
  * downloaded file.
  * This process involve a tree of URIs, represented by a list of lists.
  * Metadata is attached to every file in the tree, and the view of the
  * metadata from outside is the merging of all the metadata on the path
  * from the current active URI to the root.
  * At the end of the previous example, the tree looks like:
  * [ [ "/tmp/localfile_from_smb" ] ;
  *   [ 
  *     (* Some http://something was there but was removed without producing
  *      * anything. *)
  *     "smb://something" ; (* The successfully downloaded URI *)
  *     "ftp://another/uri" ;
  *     (* maybe some more URIs are here, ready in case of more failures *)
  *   ] ;
  *   [ "mydb://myrequest" ] (* And this is the initial URI *)
  * ]
  *)

type indicator = {
  string : string ;
  temporary : bool ;
  metadata : metadata
}

type status = Idle | Resolving | Ready | Playing | Destroyed

type t = {
  id : int ;
  initial_uri : string ;
  audio : bool ;
  persistent : bool ;

  (* The status of a request gives partial information of what's being done
   * with the request. The info is only partial because things can happen
   * in parallel. For example you can resolve a request in order to get
   * a new file from it while it is being played. For this reason, the separate
   * resolving and on_air information is not completely redundant, and do
   * not necessarily need to be part of the status information.
   * Actually this need is quite rare, and I'm not sure this is a good
   * choice. I'm wondering, so I keep the current design. *)
  mutable status : status ;
  mutable resolving : float option ;
  mutable on_air : float option ;

  log : log ;
  mutable root_metadata : metadata ;
  mutable indicators : indicator list list ;
  mutable decoder : (unit -> Decoder.decoder) option ;
}

let indicator ?(metadata=Hashtbl.create 10) ?temporary s = {
  string = home_unrelate s ;
  temporary = temporary = Some true ;
  metadata = metadata
}

(** Manage requests' metadata *)

let toplevel_metadata t =
  match t.indicators with
    | [] -> t.root_metadata
    | []::_ -> assert false
    | (h::_)::_ -> h.metadata

let iter_metadata t f =
  List.iter
    (function
       | [] -> assert false
       | h::_ -> f h.metadata)
    t.indicators ;
  f t.root_metadata

let set_metadata t k v = Hashtbl.replace (toplevel_metadata t) k v
let set_root_metadata t k v = Hashtbl.replace t.root_metadata k v

exception Found of string

let get_metadata t k =
  try
    iter_metadata t
      (fun h ->
         try raise (Found (Hashtbl.find h k)) with Not_found -> ()) ;
    (try raise (Found (Hashtbl.find t.root_metadata k)) with Not_found -> ()) ;
    None
  with
    | Found s -> Some s

let get_root_metadata t k =
  try raise (Found (Hashtbl.find t.root_metadata k)) with
    | Not_found -> None
    | Found x -> Some x

let get_all_metadata t =
  let h = Hashtbl.create 20 in
    iter_metadata t
      (Hashtbl.iter
         (fun k v -> if not (Hashtbl.mem h k) then Hashtbl.add h k v)) ;
    h

(** Logging *)

let add_log t i =
  Queue.add ((Unix.localtime (Unix.time ())),i) t.log

let get_log t = t.log

(* Indicator tree management *)

exception No_indicator

let peek_indicator t =
  match t.indicators with
    | (h::_)::_ -> h
    | []::_ -> assert false
    | [] -> raise No_indicator

let rec pop_indicator t =
  let i,repop =
    match t.indicators with
      | (h::l)::ll ->
          t.indicators <- if l = [] then ll else l::ll ;
          h,(l=[] && ll<>[])
      | []::_ -> assert false
      | [] -> raise No_indicator
  in
    if i.temporary then
      begin try
        Unix.unlink i.string
      with
        | e ->
            log#f 2 "Unlink failed: %S" (Printexc.to_string e)
      end ;
    t.decoder <- None ;
    if repop then pop_indicator t

let mresolvers_doc =
  "Methods to extract metadata from a file."
let mresolvers =
  Plug.create ~doc:mresolvers_doc ~insensitive:true "metadata formats"

let local_check t =
  if t.audio then try
    while t.decoder = None && Sys.file_exists (peek_indicator t).string do
      let indicator = peek_indicator t in
      let name = indicator.string in
        match Decoder.search_valid name with
          | Some f ->
              t.decoder <- Some f ;
              mresolvers#iter
                (fun format resolver ->
                   let ans = resolver name in
                     List.iter
                       (fun (k,v) ->
                          (* XXX Policy ? Never/always overwrite ? *)
                          Hashtbl.replace indicator.metadata
                            (String.lowercase k) (cleanup v))
                       ans) ;
              t.status <- Ready
          | None -> pop_indicator t
      done
  with
    | No_indicator -> ()

let push_indicators t l =
  if l <> [] then
    let hd = List.hd l in
      add_log t (Printf.sprintf "Pushed [%S;...]." hd.string) ;
      t.indicators <- l::t.indicators ;
      t.decoder <- None ;
      (* Performing a local check is quite fast and allows the request
       * to be instantly available if it is only made of valid local files,
       * without any need for a resolution process. *)
      (* TODO sometimes it's not that fast actually, and it'd be nice
       * to be able to disable this check in some cases, like playlist.safe. *)
      local_check t

let is_ready t =
  t.indicators <> [] &&
  Sys.file_exists (peek_indicator t).string &&
  ( t.decoder <> None || not t.audio )

(** [get_filename request] returns
  * [Some f] if the request successfully lead to a local file [f],
  * [None] otherwise. *)
let get_filename t =
  if is_ready t then
    Some (List.hd (List.hd t.indicators)).string
  else
    None

let update_metadata t =
  let replace = Hashtbl.replace t.root_metadata in
    replace "rid" (string_of_int t.id) ;
    replace "initial_uri" t.initial_uri ;
    (* TOP INDICATOR *)
    replace "temporary"
      (match t.indicators with
         | (h::_)::_ -> if h.temporary then "true" else "false"
         | _ -> "false") ;
    begin match get_filename t with
      | Some f -> replace "filename" f
      | None -> ()
    end ;
    (* STATUS *)
    begin match t.resolving with
      | Some d ->
          replace "resolving" (pretty_date (Unix.localtime d))
      | None -> ()
    end ;
    begin match t.on_air with
      | Some d ->
          replace "on_air" (pretty_date (Unix.localtime d))
      | None -> ()
    end ;
    replace "status"
      (match t.status with
         | Idle -> "idle"
         | Resolving -> "resolving"
         | Ready -> "ready"
         | Playing -> "playing"
         | Destroyed -> "destroyed")

let get_metadata t k = update_metadata t ; get_metadata t k
let get_all_metadata t = update_metadata t ; get_all_metadata t
let get_root_metadata t = update_metadata t ; get_root_metadata t

(** Request global management.
  Access to the global table is thread-safe, unlike access to a request.
  In some cases (trace) there could be a problem : TODO ? *)

let requests = Hashtbl.create Configure.requests_table_size
(* Maximum number of alive requests *)

let lock = Mutex.create ()

exception Found of int

let next =
  let request_max = conf_max_rid#get in
  let current = ref (-1) in fun () ->
    assert (not (Mutex.try_lock lock)) ;
    try
      for i = 1 to request_max do
        current := (!current+1) mod request_max ;
        begin try
          let t = Hashtbl.find requests !current in
            if t.status = Destroyed then
              raise (Found !current)
        with
          | Not_found -> raise (Found !current)
        end
      done ;
      (-1)
    with
      | Found r -> r

(** Query request state and get requests by state *)

let get_id t = t.id

let from_id id =
  Mutex.lock lock ;
  try
    let r = Hashtbl.find requests id in
      Mutex.unlock lock ;
      Some r
  with
    | Not_found -> Mutex.unlock lock ; None

let alive_requests () =
  Mutex.lock lock ;
  let l =
    Hashtbl.fold
      (fun k v l -> if v.status <> Destroyed then k::l else l)
      requests []
  in
    Mutex.unlock lock ;
    l

let is_on_air t = t.on_air <> None

let on_air_requests () =
  Mutex.lock lock ;
  let l = Hashtbl.fold (fun k v l -> if is_on_air v then k::l else l)
	    requests [] in
    Mutex.unlock lock ;
    l

let is_resolving t = t.status = Resolving

let resolving_requests () =
  Mutex.lock lock ;
  let l = Hashtbl.fold (fun k v l -> if is_resolving v then k::l else l)
	    requests [] in
    Mutex.unlock lock ;
    l

(** Creation *)

let create ?(metadata=[]) ?(audio=true) ?(persistent=false) ?(indicators=[]) u =
  Mutex.lock lock ;
  let rid = next () in
    if rid = -1 then
      ( Mutex.unlock lock ;
	log#f 2 "No available RID!" ;
	None )
    else
      let t = {
        id = rid ;
        initial_uri = u ;
        audio = audio ;
        persistent = persistent ;

        on_air = None ;
        resolving = None ;
        status = Idle ;

        decoder = None ;
        log = Queue.create () ;
        root_metadata = Hashtbl.create 10 ;
        indicators = [] }
      in
        Hashtbl.replace requests t.id t ;
        Mutex.unlock lock ;
        List.iter
          (fun (k,v) -> Hashtbl.replace t.root_metadata k v)
          metadata ;
        push_indicators t
          (if indicators=[] then [indicator u] else indicators) ;
        Some t

let on_air t =
  t.on_air <- Some (Unix.time ()) ;
  t.status <- Playing ;
  add_log t "Currently on air."

let destroy ?force t =
  assert (t.status <> Destroyed) ;
  t.on_air <- None ;
  if t.status = Playing then t.status <- Ready ;
  if force = Some true || not t.persistent then begin
    t.status <- Idle ;
    (* Freeze the metadata *)
    t.root_metadata <- get_all_metadata t ;
    (* Remove the URIs, unlink temporary files *)
    while t.indicators <> [] do pop_indicator t done ;
    t.status <- Destroyed ;
    add_log t "Request finished." ;
  end

let clean () =
  Hashtbl.iter
    (fun k r -> if r.status <> Destroyed then destroy ~force:true r)
    requests

let get_decoder r =
  match r.decoder with None -> None | Some d -> Some (d ())

(** Plugins registration. *)

type protocol = {
  resolve : string -> log:(string->unit) -> float -> indicator list ;
  static : bool ;
}

let protocols_doc =
  "Methods to get a file. They are the first part of URIs: 'protocol:args'."
let protocols = Plug.create ~doc:protocols_doc ~insensitive:true "protocols"

let is_static s =
  if Sys.file_exists (home_unrelate s) then
    Some true
  else
    let proto,arg = parse_uri s in
      match protocols#get proto with
	| Some handler -> Some handler.static
	| None -> None

(** Resolving engine. *)

type resolve_flag =
  | Resolved
  | Failed
  | Timeout

exception ExnTimeout

let resolve t timeout =
  t.resolving <- Some (Unix.time ()) ;
  t.status <- Resolving ;
  let maxtime = (Unix.time ()) +. timeout in
  let resolve_step () =
    let i = peek_indicator t in
    (* If the file is local we only need to check that it's valid,
     * we'll actually do that in a single local_check for all local indicators
     * on the top of the stack. *)
    if Sys.file_exists i.string then local_check t else
    let proto,arg = parse_uri i.string in
      match protocols#get proto with
        | Some handler ->
            add_log t (Printf.sprintf
                     "Resolving %S (timeout %.fs)..." i.string timeout) ;
            let production = handler.resolve ~log:(add_log t) arg maxtime in
              if production = [] then begin
                log#f 4
                  "Failed to resolve %S! For more info, see server command 'trace %d'."
                  i.string t.id ;
                ignore (pop_indicator t)
              end else
                push_indicators t production
        | None ->
            add_log t "Unknown protocol!" ;
            pop_indicator t
  in
  let result =
    try
      while not (is_ready t) do
	let timeleft = maxtime -. (Unix.time ()) in
	  if timeleft > 0. then
            resolve_step ()
	  else
            ( add_log t "Global timeout." ; raise ExnTimeout )
      done ;
      Resolved
    with
      | ExnTimeout -> Timeout
      | No_indicator -> add_log t "Every possibility failed!" ; Failed
  in
  let excess = (Unix.time ()) -. maxtime in
    if excess > 0. then
      log#f 2 "Time limit exceeded by %f secs!" excess ;
    t.resolving <- None ;
    if result <> Resolved then t.status <- Idle else t.status <- Ready ;
    result

(* Make a few functions more user-friendly, internal stuff is over. *)

let peek_indicator t = (peek_indicator t).string
