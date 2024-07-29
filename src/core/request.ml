(*****************************************************************************

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

(** Plug for resolving, that is obtaining a file from an URI. [src/protocols]
    plugins provide ways to resolve URIs: fetch, generate, ... *)

exception No_indicator
exception Request_resolved
exception Invalid_state
exception Duration of float

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "request") "requests configuration"

let log = Log.make ["request"]

(** File utilities. *)

let remove_file_proto s =
  (* First remove file:// 🤮 *)
  let s =
    Pcre.substitute ~rex:(Pcre.regexp "^file://") ~subst:(fun _ -> "") s
  in
  (* Then remove file: 😇 *)
  Pcre.substitute ~rex:(Pcre.regexp "^file:") ~subst:(fun _ -> "") s

let home_unrelate s = Lang_string.home_unrelate (remove_file_proto s)

let parse_uri uri =
  try
    let i = String.index uri ':' in
    Some
      (String.sub uri 0 i, String.sub uri (i + 1) (String.length uri - (i + 1)))
  with _ -> None

type resolve_flag = [ `Resolved | `Failed | `Timeout ]

type metadata_resolver = {
  priority : unit -> int;
  resolver :
    metadata:Frame.metadata ->
    extension:string option ->
    mime:string ->
    string ->
    (string * string) list;
}

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

type indicator = { uri : string; temporary : bool; metadata : Frame.metadata }
type resolving = { until : float; pending : (Condition.t * Mutex.t) list }
type status = [ `Idle | `Resolving of resolving | `Ready | `Destroyed | `Failed ]
type decoder = string * (unit -> Decoder.file_decoder_ops)

type t = {
  id : int;
  resolve_metadata : bool;
  excluded_metadata_resolvers : string list;
  cue_in_metadata : string option;
  cue_out_metadata : string option;
  persistent : bool;
  decoders : (Frame.content_type, decoder option) Hashtbl.t;
  status : status Atomic.t;
  logger : Log.t;
  log : log;
  mutable indicators : indicator list;
  mutable file_metadata : Frame.Metadata.t;
}

let resolved t = match Atomic.get t.status with `Ready -> true | _ -> false

let initial_uri r =
  match List.rev r.indicators with { uri } :: _ -> uri | [] -> assert false

let status { status } = Atomic.get status

let indicator ?(metadata = Frame.Metadata.empty) ?temporary s =
  { uri = home_unrelate s; temporary = temporary = Some true; metadata }

(** Length *)
let dresolvers_doc = "Methods to extract duration from a file."

let dresolvers = Plug.create ~doc:dresolvers_doc "audio file formats (duration)"

let compute_duration ~metadata file =
  try
    Plug.iter dresolvers (fun _ resolver ->
        try
          let ans = resolver ~metadata file in
          raise (Duration ans)
        with
          | Duration e -> raise (Duration e)
          | _ -> ());
    raise Not_found
  with Duration d -> d

let duration ~metadata file =
  try
    match
      ( Frame.Metadata.find_opt "duration" metadata,
        Frame.Metadata.find_opt "cue_in" metadata,
        Frame.Metadata.find_opt "cue_out" metadata )
    with
      | _, Some cue_in, Some cue_out ->
          Some (float_of_string cue_out -. float_of_string cue_in)
      | _, None, Some cue_out -> Some (float_of_string cue_out)
      | Some v, _, _ -> Some (float_of_string v)
      | None, cue_in, None ->
          let duration = compute_duration ~metadata file in
          let duration =
            match cue_in with
              | Some cue_in -> duration -. float_of_string cue_in
              | None -> duration
          in
          Some duration
  with _ -> None

(** [get_filename request] returns
  * [Some f] if the request successfully lead to a local file [f],
  * [None] otherwise. *)
let get_filename t =
  if resolved t then Some (List.hd t.indicators).uri else None

(** Manage requests' metadata *)

let add_root_metadata t m =
  let m = Frame.Metadata.add "rid" (string_of_int t.id) m in
  let m = Frame.Metadata.add "initial_uri" (initial_uri t) m in

  (* TOP INDICATOR *)
  let m =
    Frame.Metadata.add "temporary"
      (match t.indicators with
        | h :: _ -> if h.temporary then "true" else "false"
        | _ -> "false")
      m
  in

  let m =
    match get_filename t with
      | Some f -> Frame.Metadata.add "filename" f m
      | None -> m
  in

  (* STATUS *)
  match Atomic.get t.status with
    | `Idle -> Frame.Metadata.add "status" "idle" m
    | `Resolving { until } ->
        let m =
          Frame.Metadata.add "resolving" (pretty_date (Unix.localtime until)) m
        in
        Frame.Metadata.add "status" "resolving" m
    | `Ready -> Frame.Metadata.add "status" "ready" m
    | `Destroyed -> Frame.Metadata.add "status" "destroyed" m
    | `Failed -> Frame.Metadata.add "status" "failed" m

let metadata t =
  add_root_metadata t
    (List.fold_left
       (fun m h -> Frame.Metadata.append h.metadata m)
       t.file_metadata (List.rev t.indicators))

(** Logging *)

let add_log t i =
  t.logger#info "%s" i;
  Queue.add (Unix.localtime (Unix.time ()), i) t.log

let get_log t = t.log

(* Indicator tree management *)

let () =
  Printexc.register_printer (function
    | No_indicator -> Some "All options exhausted while processing request"
    | _ -> None)

let string_of_indicators t =
  let i = t.indicators in
  let string_of_list l = "[" ^ String.concat ", " l ^ "]" in
  let i = (List.map (fun i -> i.uri)) i in
  string_of_list i

let conf_metadata_decoders =
  Dtools.Conf.list
    ~p:(conf#plug "metadata_decoders")
    ~d:[] "Decoders and order used to decode files' metadata."

let conf_metadata_decoder_priorities =
  Dtools.Conf.void
    ~p:(conf_metadata_decoders#plug "priorities")
    "Priorities used for applying metadata decoders. Decoder with the highest \
     priority take precedence."

let conf_request_metadata_priority =
  Dtools.Conf.int ~d:5
    ~p:(conf_metadata_decoder_priorities#plug "request_metadata")
    "Priority for the request metadata. This include metadata set via \
     `annotate`."

let f c v =
  match c#get_d with
    | None -> c#set_d (Some [v])
    | Some d -> c#set_d (Some (d @ [v]))

let get_decoders conf decoders =
  let f cur name =
    match Plug.get decoders name with
      | Some p -> (name, p) :: cur
      | None ->
          log#severe "Cannot find decoder %s" name;
          cur
  in
  List.sort
    (fun (_, d) (_, d') -> Stdlib.compare (d'.priority ()) (d.priority ()))
    (List.fold_left f [] (List.rev conf#get))

let mresolvers_doc = "Methods to extract metadata from a file."

let mresolvers =
  Plug.create
    ~register_hook:(fun name _ -> f conf_metadata_decoders name)
    ~doc:mresolvers_doc "metadata formats"

let conf_duration =
  Dtools.Conf.bool
    ~p:(conf_metadata_decoders#plug "duration")
    ~d:false
    "Compute duration in the \"duration\" metadata, if the metadata is not \
     already present. This can take a long time and the use of this option is \
     not recommended: the proper way is to have a script precompute the \
     \"duration\" metadata."

let conf_recode =
  Dtools.Conf.bool
    ~p:(conf_metadata_decoders#plug "recode")
    ~d:true "Re-encode metadata strings in UTF-8"

let conf_recode_excluded =
  Dtools.Conf.list
    ~d:["apic"; "metadata_block_picture"; "coverart"]
    ~p:(conf_recode#plug "exclude")
    "Exclude these metadata from automatic recording."

let resolve_metadata ~initial_metadata ~excluded name =
  let decoders = get_decoders conf_metadata_decoders mresolvers in
  let decoders =
    List.filter (fun (name, _) -> not (List.mem name excluded)) decoders
  in
  let high_priority_decoders, low_priority_decoders =
    List.partition
      (fun (_, { priority }) ->
        conf_request_metadata_priority#get < priority ())
      decoders
  in
  let convert =
    if conf_recode#get then (
      let excluded = conf_recode_excluded#get in
      fun k v -> if not (List.mem k excluded) then Charset.convert v else v)
    else fun _ x -> x
  in
  let extension = try Some (Utils.get_ext name) with _ -> None in
  let mime = Magic_mime.lookup name in
  let get_metadata ~metadata decoders =
    List.fold_left
      (fun metadata (_, { resolver }) ->
        try
          let ans = resolver ~metadata:initial_metadata ~extension ~mime name in
          List.fold_left
            (fun metadata (k, v) ->
              let k = String.lowercase_ascii (convert k k) in
              let v = convert k v in
              if not (Frame.Metadata.mem k metadata) then
                Frame.Metadata.add k v metadata
              else metadata)
            metadata ans
        with _ -> metadata)
      metadata decoders
  in
  let metadata =
    get_metadata ~metadata:Frame.Metadata.empty high_priority_decoders
  in
  let metadata =
    get_metadata
      ~metadata:(Frame.Metadata.append initial_metadata metadata)
      low_priority_decoders
  in
  if conf_duration#get then (
    match duration ~metadata name with
      | None -> metadata
      | Some d -> Frame.Metadata.add "duration" (string_of_float d) metadata)
  else metadata

(** Sys.file_exists doesn't make a difference between existing files and files
    without enough permissions to list their attributes, for example when they
    are in a directory without x permission.  The two following functions allow a
    more precise diagnostic.  We do not use them everywhere in this file, but
    only when splitting existence and readability checks yields better logs. *)

let file_exists name =
  try
    Unix.access name [Unix.F_OK];
    true
  with
    | Unix.Unix_error (Unix.EACCES, _, _) -> true
    | Unix.Unix_error _ -> false

let file_is_readable name =
  try
    Unix.access name [Unix.R_OK];
    true
  with Unix.Unix_error _ -> false

let read_metadata r =
  let i = List.hd r.indicators in
  let metadata =
    resolve_metadata ~initial_metadata:(metadata r)
      ~excluded:r.excluded_metadata_resolvers i.uri
  in
  r.file_metadata <- metadata

let push_indicator t i =
  add_log t (Printf.sprintf "Pushed [%s;...]." (Lang_string.quote_string i.uri));
  t.indicators <- i :: t.indicators

(** Global management *)

module Pool = Pool.Make (struct
  type req = t
  type t = req

  let id { id } = id

  let destroyed =
    {
      id = 0;
      cue_in_metadata = None;
      cue_out_metadata = None;
      resolve_metadata = false;
      excluded_metadata_resolvers = [];
      persistent = false;
      status = Atomic.make `Destroyed;
      logger = Log.make [];
      log = Queue.create ();
      decoders = Hashtbl.create 1;
      indicators = [];
      file_metadata = Frame.Metadata.empty;
    }

  let destroyed id = { destroyed with id }
  let is_destroyed { status } = Atomic.get status = `Destroyed
end)

let id { id } = id
let from_id id = Pool.find id
let all () = Pool.fold (fun _ r l -> r :: l) []

(** Creation *)

let leak_warning =
  Dtools.Conf.int ~p:(conf#plug "leak_warning") ~d:100
    "Number of requests at which a leak warning should be issued."

let destroy ?force t =
  if Atomic.get t.status <> `Destroyed then
    if force = Some true || not t.persistent then (
      List.iter
        (fun i ->
          if i.temporary && file_exists i.uri then (
            try Unix.unlink i.uri
            with e -> log#severe "Unlink failed: %S" (Printexc.to_string e)))
        t.indicators;

      (* Keep the first indicator as initial_uri .*)
      t.indicators <- [List.hd (List.rev t.indicators)];
      Hashtbl.reset t.decoders;
      Atomic.set t.status `Destroyed;
      add_log t "Request destroyed.")

let finalise = destroy ~force:true

let cleanup () =
  Pool.iter (fun _ r ->
      if Atomic.get r.status <> `Destroyed then destroy ~force:true r);
  Pool.clear ()

let create ?(resolve_metadata = true) ?(excluded_metadata_resolvers = [])
    ?metadata ?(persistent = false) ?temporary ~cue_in_metadata
    ~cue_out_metadata uri =
  (* Find instantaneous request loops *)
  let n = Pool.size () in
  if n > 0 && n mod leak_warning#get = 0 then
    log#severe
      "There are currently %d RIDs, possible request leak! Please check that \
       you don't have a loop on empty/unavailable requests."
      n;
  let t =
    let req =
      {
        id = 0;
        cue_in_metadata;
        cue_out_metadata;
        resolve_metadata;
        excluded_metadata_resolvers;
        (* This is fixed when resolving the request. *)
        persistent;
        status = Atomic.make `Idle;
        logger = Log.make [];
        log = Queue.create ();
        decoders = Hashtbl.create 1;
        indicators = [];
        file_metadata = Frame.Metadata.empty;
      }
    in
    Pool.add (fun id ->
        { req with id; logger = Log.make ["request"; string_of_int id] })
  in
  push_indicator t (indicator ?metadata ?temporary uri);
  Gc.finalise finalise t;
  t

let get_cue ~r = function
  | None -> None
  | Some m -> (
      match Frame.Metadata.find_opt m (metadata r) with
        | None -> None
        | Some v -> (
            match float_of_string_opt v with
              | None ->
                  r.logger#important "Invalid cue metadata %s: %s" m v;
                  None
              | Some v -> Some v))

exception Found_decoder of decoder option

let get_base_decoder ~ctype r =
  if not (resolved r) then None
  else (
    try
      Hashtbl.iter
        (fun c d -> if Frame.compatible c ctype then raise (Found_decoder d))
        r.decoders;
      let filename = (List.hd r.indicators).uri in
      let metadata = metadata r in
      let d = Decoder.get_file_decoder ~metadata ~ctype filename in
      Hashtbl.replace r.decoders ctype d;
      d
    with Found_decoder d -> d)

let has_decoder ~ctype r = get_base_decoder ~ctype r <> None

let get_decoder ~ctype r =
  match get_base_decoder ~ctype r with
    | None -> None
    | Some (_, d) -> (
        let decoder = d () in
        let open Decoder in
        let initial_pos =
          match get_cue ~r r.cue_in_metadata with
            | Some cue_in ->
                r.logger#info "Cueing in to position: %.02f" cue_in;
                let cue_in = Frame.main_of_seconds cue_in in
                let seeked = decoder.fseek cue_in in
                if seeked <> cue_in then
                  r.logger#important
                    "Initial seek mismatch! Expected: %d, effective: %d" cue_in
                    seeked;
                seeked
            | None -> 0
        in
        match get_cue ~r r.cue_out_metadata with
          | None -> Some decoder
          | Some cue_out ->
              let cue_out = Frame.main_of_seconds cue_out in
              let pos = Atomic.make initial_pos in
              let fread len =
                if cue_out <= Atomic.get pos then decoder.fread 0
                else (
                  let old_pos = Atomic.get pos in
                  let len = min len (cue_out - old_pos) in
                  let buf = decoder.fread len in
                  let filled = Frame.position buf in
                  let new_pos = old_pos + filled in
                  Atomic.set pos new_pos;
                  if cue_out <= new_pos then (
                    r.logger#info "Cueing out at position: %.02f"
                      (Frame.seconds_of_main cue_out);
                    Frame.slice buf (cue_out - old_pos))
                  else (
                    if Frame.is_partial buf then
                      r.logger#important
                        "End of track reached before cue-out point!";
                    buf))
              in
              let remaining () =
                match (decoder.remaining (), cue_out - Atomic.get pos) with
                  | -1, r -> r
                  | r, r' -> min r r'
              in
              Some { decoder with fread; remaining })

(** Plugins registration. *)

type resolver = string -> log:(string -> unit) -> float -> indicator option
type protocol = { resolve : resolver; static : bool }

let protocols_doc =
  "Methods to get a file. They are the first part of URIs: 'protocol:args'."

let protocols = Plug.create ~doc:protocols_doc "protocols"

let is_static s =
  if file_exists (home_unrelate s) then true
  else (
    match parse_uri s with
      | Some (proto, _) -> (
          match Plug.get protocols proto with
            | Some handler -> handler.static
            | None -> false)
      | None -> false)

(** Resolving engine. *)

exception ExnTimeout

let should_fail = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"Requests shutdown" (fun () ->
      Atomic.set should_fail true)

let resolve_req t timeout =
  log#debug "Resolving request %s." (string_of_indicators t);
  Atomic.set t.status (`Resolving { until = Unix.time (); pending = [] });
  let maxtime = Unix.time () +. timeout in
  let rec resolve i =
    if Atomic.get should_fail then raise No_indicator;
    let timeleft = maxtime -. Unix.time () in
    if timeleft <= 0. then (
      add_log t "Global timeout.";
      raise ExnTimeout);

    log#f 6 "Resolve step %s in %s." i.uri (string_of_indicators t);
    (* If the file is local, this loop should always terminate. *)
    if file_exists i.uri then (
      if not (file_is_readable i.uri) then (
        log#important "Read permission denied for %s!"
          (Lang_string.quote_string i.uri);
        add_log t "Read permission denied!";
        raise No_indicator);
      if t.resolve_metadata then read_metadata t;
      raise Request_resolved);

    match parse_uri i.uri with
      | Some (proto, arg) -> (
          match Plug.get protocols proto with
            | Some handler -> (
                add_log t
                  (Printf.sprintf "Resolving %s (timeout %.0fs)..."
                     (Lang_string.quote_string i.uri)
                     timeout);
                match handler.resolve ~log:(add_log t) arg maxtime with
                  | None ->
                      log#info
                        "Failed to resolve %s! For more info, see server \
                         command `request.trace %d`."
                        (Lang_string.quote_string i.uri)
                        t.id;
                      raise No_indicator
                  | Some i ->
                      push_indicator t i;
                      resolve i)
            | None ->
                log#important "Unknown protocol %S in URI %s!" proto
                  (Lang_string.quote_string i.uri);
                add_log t "Unknown protocol!";
                raise No_indicator)
      | None ->
          let log_level = if i.uri = "" then 4 else 3 in
          log#f log_level "Nonexistent file or ill-formed URI %s!"
            (Lang_string.quote_string i.uri);
          add_log t "Nonexistent file or ill-formed URI!";
          raise No_indicator
  in
  let result =
    try
      if Atomic.get should_fail then raise No_indicator;
      match t.indicators with i :: [] -> resolve i | _ -> assert false
    with
      | Request_resolved -> `Resolved
      | ExnTimeout -> `Timeout
      | No_indicator ->
          add_log t "Every possibility failed!";
          `Failed
  in
  log#debug "Resolved to %s." (string_of_indicators t);
  let excess = Unix.time () -. maxtime in
  if excess > 0. then log#severe "Time limit exceeded by %.2f secs!" excess;
  let status = if result <> `Resolved then `Failed else `Ready in
  (match Atomic.exchange t.status status with
    | `Resolving { pending } ->
        List.iter
          (fun (c, m) ->
            Mutex_utils.mutexify m (fun () -> Condition.signal c) ())
          pending
    | _ -> assert false);
  result

let rec resolve t timeout =
  match Atomic.get t.status with
    | `Idle -> resolve_req t timeout
    | `Resolving ({ pending } as r) as status ->
        let m = Mutex.create () in
        let c = Condition.create () in
        Mutex_utils.mutexify m
          (fun () ->
            if
              Atomic.compare_and_set t.status status
                (`Resolving { r with pending = (c, m) :: pending })
            then Condition.wait c m)
          ();
        resolve t timeout
    | `Ready -> `Resolved
    | `Destroyed | `Failed -> `Failed

(* Make a few functions more user-friendly, internal stuff is over. *)

module Value = Value.MkCustom (struct
  type content = t

  let name = "request"

  let to_json ~pos _ =
    Runtime_error.raise ~pos ~message:"Requests cannot be represented as json"
      "json"

  let to_string r = Printf.sprintf "<request(id=%d)>" r.id
  let compare r r' = Stdlib.compare r.id r'.id
end)
