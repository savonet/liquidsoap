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

(** Plug for resolving, that is obtaining a file from an URI.
  * [src/protocols] plugins provide ways
  * to resolve URIs: fetch, generate, ... *)

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "request") "requests configuration"

let grace_time =
  Dtools.Conf.float ~p:(conf#plug "grace_time") ~d:600.
    "Time (in seconds) after which a destroyed request cannot be accessed \
     anymore."

let log = Log.make ["request"]

(** File utilities. *)

let remove_file_proto s =
  Pcre.substitute ~pat:"^file://" ~subst:(fun _ -> "") s

let home_unrelate s = Utils.home_unrelate (remove_file_proto s)

let parse_uri uri =
  try
    let i = String.index uri ':' in
    Some
      (String.sub uri 0 i, String.sub uri (i + 1) (String.length uri - (i + 1)))
  with _ -> None

let cleanup =
  let re1 = Str.regexp "^[\t ]*" in
  let re2 = Str.regexp "[\t ]*$" in
  fun s -> Str.global_replace re1 "" (Str.global_replace re2 "" s)

(** Metadata *)

type metadata = (string, string) Hashtbl.t

let string_of_metadata metadata =
  let b = Buffer.create 20 in
  let f = Format.formatter_of_buffer b in
  let escape f s = Utils.escape_utf8 f s in
  let first = ref true in
  Hashtbl.iter
    (fun k v ->
      if !first then (
        first := false ;
        try Format.fprintf f "%s=%a" k escape v with _ -> () )
      else (try Format.fprintf f "\n%s=%a" k escape v with _ -> ()))
    metadata ;
  Format.pp_print_flush f () ;
  Buffer.contents b

let short_string_of_metadata m =
  "Title: "
  ^
  try
    let t = Hashtbl.find m "title" in
    if String.length t < 12 then t else String.sub t 0 9 ^ "..."
  with Not_found -> "(undef)"

(** Log *)

type log = (Unix.tm * string) Queue.t

let pretty_date date =
  Printf.sprintf "%d/%02d/%02d %02d:%02d:%02d" (date.Unix.tm_year + 1900)
    (date.Unix.tm_mon + 1) date.Unix.tm_mday date.Unix.tm_hour date.Unix.tm_min
    date.Unix.tm_sec

let string_of_log log =
  Queue.fold
    (fun s (date, msg) ->
      s
      ^
      if s = "" then Printf.sprintf "[%s] %s" (pretty_date date) msg
      else Printf.sprintf "\n[%s] %s" (pretty_date date) msg)
    "" log

(** Requests.
  * The purpose of a request is to get a valid file. The file can contain media
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

type indicator = {string: string; temporary: bool; metadata: metadata}

type status = Idle | Resolving | Ready | Playing | Destroyed

type t = {
  id: int;
  initial_uri: string;
  kind: Frame.content_kind option;
  (* No kind for raw requests *)
  persistent: bool;
  (* The status of a request gives partial information of what's being done
   * with the request. The info is only partial because things can happen
   * in parallel. For example you can resolve a request in order to get
   * a new file from it while it is being played. For this reason, the separate
   * resolving and on_air information is not completely redundant, and do
   * not necessarily need to be part of the status information.
   * Actually this need is quite rare, and I'm not sure this is a good
   * choice. I'm wondering, so I keep the current design. *)
  mutable status: status;
  mutable resolving: float option;
  mutable on_air: float option;
  log: log;
  mutable root_metadata: metadata;
  mutable indicators: indicator list list;
  mutable decoder: (unit -> Decoder.file_decoder) option;
}

let kind x = x.kind

let initial_uri x = x.initial_uri

let indicator ?(metadata = Hashtbl.create 10) ?temporary s =
  {string= home_unrelate s; temporary= temporary = Some true; metadata}

(** Length *)
let dresolvers_doc = "Methods to extract duration from a file."

let dresolvers =
  Plug.create ~doc:dresolvers_doc ~insensitive:true
    "audio file formats (duration)"

exception Duration of float

let duration file =
  try
    dresolvers#iter (fun _ resolver ->
        try
          let ans = resolver file in
          raise (Duration ans)
        with
          | Duration e ->
              raise (Duration e)
          | _ ->
              ()) ;
    raise Not_found
  with Duration d -> d

(** Manage requests' metadata *)

let toplevel_metadata t =
  match t.indicators with
    | [] ->
        t.root_metadata
    | [] :: _ ->
        assert false
    | (h :: _) :: _ ->
        h.metadata

let iter_metadata t f =
  List.iter
    (function [] -> assert false | h :: _ -> f h.metadata)
    t.indicators ;
  f t.root_metadata

let set_metadata t k v = Hashtbl.replace (toplevel_metadata t) k v

let set_root_metadata t k v = Hashtbl.replace t.root_metadata k v

exception Found of string

let get_metadata t k =
  try
    iter_metadata t (fun h ->
        try raise (Found (Hashtbl.find h k)) with Not_found -> ()) ;
    (try raise (Found (Hashtbl.find t.root_metadata k)) with Not_found -> ()) ;
    None
  with Found s -> Some s

let get_root_metadata t k =
  try raise (Found (Hashtbl.find t.root_metadata k)) with
    | Not_found ->
        None
    | Found x ->
        Some x

let get_all_metadata t =
  let h = Hashtbl.create 20 in
  iter_metadata t
    (Hashtbl.iter (fun k v -> if not (Hashtbl.mem h k) then Hashtbl.add h k v)) ;
  h

(** Logging *)

let add_log t i = Queue.add (Unix.localtime (Unix.time ()), i) t.log

let get_log t = t.log

(* Indicator tree management *)

exception No_indicator

let () =
  Printexc.register_printer (function
    | No_indicator ->
        Some "All options exhausted while processing request"
    | _ ->
        None)

let peek_indicator t =
  match t.indicators with
    | (h :: _) :: _ ->
        h
    | [] :: _ ->
        assert false
    | [] ->
        raise No_indicator

let rec pop_indicator t =
  let i, repop =
    match t.indicators with
      | (h :: l) :: ll ->
          t.indicators <- (if l = [] then ll else l :: ll) ;
          (h, l = [] && ll <> [])
      | [] :: _ ->
          assert false
      | [] ->
          raise No_indicator
  in
  if i.temporary then (
    try Unix.unlink i.string
    with e -> log#severe "Unlink failed: %S" (Printexc.to_string e) ) ;
  t.decoder <- None ;
  if repop then pop_indicator t

let conf_metadata_decoders =
  Dtools.Conf.list
    ~p:(conf#plug "metadata_decoders")
    ~d:[] "Decoders and order used to decode files' metadata."

let f c v =
  match c#get_d with
    | None ->
        c#set_d (Some [v])
    | Some d ->
        c#set_d (Some (d @ [v]))

let get_decoders conf decoders =
  let f cur name =
    match decoders#get name with
      | Some p ->
          (name, p) :: cur
      | None ->
          log#severe "Cannot find decoder %s" name ;
          cur
  in
  List.fold_left f [] (List.rev conf#get)

let mresolvers_doc = "Methods to extract metadata from a file."

let mresolvers =
  Plug.create
    ~register_hook:(fun (name, _) -> f conf_metadata_decoders name)
    ~doc:mresolvers_doc ~insensitive:true "metadata formats"

let conf_override_metadata =
  Dtools.Conf.bool
    ~p:(conf_metadata_decoders#plug "override")
    ~d:false
    "Allow metadata resolvers to override metadata already set through \
     annotate: or playlist resolution for instance."

let conf_duration =
  Dtools.Conf.bool
    ~p:(conf_metadata_decoders#plug "duration")
    ~d:false
    "Compute duration in the \"duration\" metadata, if the metadata is not \
     already present. This can take a long time and the use of this option is \
     not recommended: the proper way is to have a script precompute the \
     \"duration\" metadata."

(** Sys.file_exists doesn't make a difference between existing files
  * and files without enough permissions to list their attributes,
  * for example when they are in a directory without x permission.
  * The two following functions allow a more precise diagnostic.
  * We do not use them everywhere in this file, but only when splitting
  * existence and readability checks yields better logs. *)

let file_exists name =
  try
    Unix.access name [Unix.F_OK] ;
    true
  with
    | Unix.Unix_error (Unix.EACCES, _, _) ->
        true
    | Unix.Unix_error _ ->
        false

let file_is_readable name =
  try
    Unix.access name [Unix.R_OK] ;
    true
  with Unix.Unix_error _ -> false

let local_check t =
  let check_decodable kind =
    try
      while t.decoder = None && file_exists (peek_indicator t).string do
        let indicator = peek_indicator t in
        let name = indicator.string in
        let metadata = get_all_metadata t in
        if not (file_is_readable name) then (
          log#important "Read permission denied for %S!" name ;
          add_log t "Read permission denied!" ;
          pop_indicator t )
        else (
          match Decoder.get_file_decoder ~metadata name kind with
            | Some (decoder_name, f) ->
                t.decoder <- Some f ;
                set_root_metadata t "decoder" decoder_name ;
                List.iter
                  (fun (_, resolver) ->
                    try
                      let ans = resolver name in
                      List.iter
                        (fun (k, v) ->
                          let k = String.lowercase_ascii k in
                          if
                            conf_override_metadata#get
                            || get_metadata t k = None
                          then Hashtbl.replace indicator.metadata k (cleanup v))
                        ans ;
                      if conf_duration#get && get_metadata t "duration" = None
                      then (
                        try
                          Hashtbl.replace indicator.metadata "duration"
                            (string_of_float (duration name))
                        with Not_found -> () )
                    with _ -> ())
                  (get_decoders conf_metadata_decoders mresolvers) ;
                t.status <- Ready
            | None ->
                pop_indicator t )
      done
    with No_indicator -> ()
  in
  match t.kind with None -> () | Some k -> check_decodable k

let push_indicators t l =
  if l <> [] then (
    let hd = List.hd l in
    add_log t (Printf.sprintf "Pushed [%S;...]." hd.string) ;
    t.indicators <- l :: t.indicators ;
    t.decoder <- None ;
    (* Performing a local check is quite fast and allows the request
     * to be instantly available if it is only made of valid local files,
     * without any need for a resolution process. *)
    (* TODO sometimes it's not that fast actually, and it'd be nice
     * to be able to disable this check in some cases, like playlist.safe. *)
    local_check t )

let is_ready t =
  t.indicators <> []
  && Sys.file_exists (peek_indicator t).string
  && (t.decoder <> None || t.kind = None)

(** [get_filename request] returns
  * [Some f] if the request successfully lead to a local file [f],
  * [None] otherwise. *)
let get_filename t =
  if is_ready t then Some (List.hd (List.hd t.indicators)).string else None

let update_metadata t =
  let replace = Hashtbl.replace t.root_metadata in
  replace "rid" (string_of_int t.id) ;
  replace "initial_uri" t.initial_uri ;
  (* TOP INDICATOR *)
  replace "temporary"
    ( match t.indicators with
      | (h :: _) :: _ ->
          if h.temporary then "true" else "false"
      | _ ->
          "false" ) ;
  begin
    match get_filename t with Some f -> replace "filename" f | None -> ()
  end ;
  (* STATUS *)
  begin
    match t.resolving with Some d ->
        replace "resolving" (pretty_date (Unix.localtime d))
    | None -> ()
  end ;
  begin
    match t.on_air with Some d ->
        replace "on_air" (pretty_date (Unix.localtime d))
    | None -> ()
  end ;
  begin
    match t.kind with None -> () | Some k ->
        replace "kind" (Frame.string_of_content_kind k)
  end ;
  replace "status"
    ( match t.status with
      | Idle ->
          "idle"
      | Resolving ->
          "resolving"
      | Ready ->
          "ready"
      | Playing ->
          "playing"
      | Destroyed ->
          "destroyed" )

let get_metadata t k = update_metadata t ; get_metadata t k

let get_all_metadata t = update_metadata t ; get_all_metadata t

let get_root_metadata t = update_metadata t ; get_root_metadata t

(** Global management *)

module Pool = Pool.Make (struct
  type req = t

  type t = req
end)

let get_id t = t.id

let from_id id = Pool.find id

let all_requests () = Pool.fold (fun k _ l -> k :: l) []

let alive_requests () =
  Pool.fold (fun k v l -> if v.status <> Destroyed then k :: l else l) []

let is_on_air t = t.on_air <> None

let on_air_requests () =
  Pool.fold (fun k v l -> if is_on_air v then k :: l else l) []

let is_resolving t = t.status = Resolving

let resolving_requests () =
  Pool.fold (fun k v l -> if is_resolving v then k :: l else l) []

(** Creation *)

let leak_warning =
  Dtools.Conf.int ~p:(conf#plug "leak_warning") ~d:100
    "Number of requests at which a leak warning should be issued."

let create ~kind ?(metadata = []) ?(persistent = false) ?(indicators = []) u =
  (* Find instantaneous request loops *)
  let () =
    let n = Pool.size () in
    if n > 0 && n mod leak_warning#get = 0 then
      log#severe
        "There are currently %d RIDs, possible request leak! Please check \
         that you don't have a loop on empty/unavailable requests, or \
         creating requests without destroying them. Decreasing \
         request.grace_time can also help."
        n
  in
  let rid, register = Pool.add () in
  let t =
    {
      id= rid;
      initial_uri= u;
      kind;
      persistent;
      on_air= None;
      resolving= None;
      status= Idle;
      decoder= None;
      log= Queue.create ();
      root_metadata= Hashtbl.create 10;
      indicators= [];
    }
  in
  register t ;
  List.iter (fun (k, v) -> Hashtbl.replace t.root_metadata k v) metadata ;
  push_indicators t (if indicators = [] then [indicator u] else indicators) ;
  t

let create_raw = create ~kind:None

let create ~kind = create ~kind:(Some kind)

let on_air t =
  t.on_air <- Some (Unix.time ()) ;
  t.status <- Playing ;
  add_log t "Currently on air."

let destroy ?force t =
  assert (t.status <> Destroyed) ;
  t.on_air <- None ;
  if t.status = Playing then t.status <- Ready ;
  if force = Some true || not t.persistent then (
    t.status <- Idle ;
    (* Freeze the metadata *)
    t.root_metadata <- get_all_metadata t ;
    (* Remove the URIs, unlink temporary files *)
    while t.indicators <> [] do
      pop_indicator t
    done ;
    t.status <- Destroyed ;
    add_log t "Request finished." ;
    Pool.kill t.id grace_time#get )

let clean () =
  Pool.iter (fun _ r -> if r.status <> Destroyed then destroy ~force:true r)

let get_decoder r = match r.decoder with None -> None | Some d -> Some (d ())

(** Plugins registration. *)

type resolver = string -> log:(string -> unit) -> float -> indicator list

type protocol = {
  resolve: string -> log:(string -> unit) -> float -> indicator list;
  static: bool;
}

let protocols_doc =
  "Methods to get a file. They are the first part of URIs: 'protocol:args'."

let protocols = Plug.create ~doc:protocols_doc ~insensitive:true "protocols"

let is_static s =
  if Sys.file_exists (home_unrelate s) then true
  else (
    match parse_uri s with
      | Some (proto, _) -> (
        match protocols#get proto with
          | Some handler ->
              handler.static
          | None ->
              false )
      | None ->
          false )

(** Resolving engine. *)

type resolve_flag = Resolved | Failed | Timeout

exception ExnTimeout

let resolve t timeout =
  t.resolving <- Some (Unix.time ()) ;
  t.status <- Resolving ;
  let maxtime = Unix.time () +. timeout in
  let resolve_step () =
    let i = peek_indicator t in
    (* If the file is local we only need to check that it's valid,
     * we'll actually do that in a single local_check for all local indicators
     * on the top of the stack. *)
    if file_exists i.string then local_check t
    else (
      match parse_uri i.string with
        | Some (proto, arg) -> (
          match protocols#get proto with
            | Some handler ->
                add_log t
                  (Printf.sprintf "Resolving %S (timeout %.0fs)..." i.string
                     timeout) ;
                let production =
                  handler.resolve ~log:(add_log t) arg maxtime
                in
                if production = [] then (
                  log#info
                    "Failed to resolve %S! For more info, see server command \
                     'trace %d'."
                    i.string t.id ;
                  ignore (pop_indicator t) )
                else push_indicators t production
            | None ->
                log#important "Unknown protocol %S in URI %S!" proto i.string ;
                add_log t "Unknown protocol!" ;
                pop_indicator t )
        | None ->
            let log_level = if i.string = "" then 4 else 3 in
            log#f log_level "Nonexistent file or ill-formed URI %S!" i.string ;
            add_log t "Nonexistent file or ill-formed URI!" ;
            pop_indicator t )
  in
  let result =
    try
      while not (is_ready t) do
        let timeleft = maxtime -. Unix.time () in
        if timeleft > 0. then resolve_step ()
        else (
          add_log t "Global timeout." ;
          raise ExnTimeout )
      done ;
      Resolved
    with
      | ExnTimeout ->
          Timeout
      | No_indicator ->
          add_log t "Every possibility failed!" ;
          Failed
  in
  let excess = Unix.time () -. maxtime in
  if excess > 0. then log#severe "Time limit exceeded by %.2f secs!" excess ;
  t.resolving <- None ;
  if result <> Resolved then t.status <- Idle else t.status <- Ready ;
  result

(* Make a few functions more user-friendly, internal stuff is over. *)

let peek_indicator t = (peek_indicator t).string
