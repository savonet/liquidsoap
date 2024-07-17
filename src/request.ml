
(** Plug for resolving, that is obtaining a file from an URI.
  * [src/protocols] plugins provide ways
  * to resolve URIs: fetch, generate, ... *)

open Dtools

let home =
  try Sys.getenv "HOME" with Not_found -> "/nonexistent"

(** File utilities. *)

let home_unrelate s =
  if String.length s >= 2 then
    match (s.[0] = '~'), (s.[1] = '/') with
      | true, true ->  (* Something like ~/data/file *)
	  Filename.concat home (String.sub s 2 (String.length s - 2))
      | true, false -> (* Something like ~bob/data/file *)
	  (Unix.getpwnam (String.sub s 1 (String.length s - 1))).Unix.pw_dir
      | _ -> s
  else
    s

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

(** Log and metadatas. *)

type metadata = (string,string) Hashtbl.t
type log = (Unix.tm * string) Queue.t

let string_of_metadata metadatas =
  Hashtbl.fold (fun k v s ->
		  s^(Printf.sprintf "%s=%S\n" k v)) metadatas ""

let short_string_of_metadata m =
  "Title: "^(
    try
      let t = Hashtbl.find m "title" in
	if String.length t < 12 then t else
	  (String.sub t 0 9)^"..."
    with
      | Not_found -> "(undef)"
  )

let pretty_date date =
  Printf.sprintf "%d/%d/%d %02d:%02d:%02d"
    (date.Unix.tm_year+1900)
    (date.Unix.tm_mon+1) date.Unix.tm_mday
    date.Unix.tm_hour date.Unix.tm_min date.Unix.tm_sec

let string_of_log log =
  Queue.fold (fun s (date,msg) ->
		s^(Printf.sprintf "[%s] %s\n" (pretty_date date) msg)) "" log

(** Requests. *)

type t = {
  id : int ;
  audio : bool ;
  mutable destroyed : bool ;
  log : log ;
  mutable indicator : string list ;
  metadata : metadata }

let set_metadata t k v = Hashtbl.replace t.metadata k v
let get_metadata t k =
  try Some (Hashtbl.find t.metadata k) with Not_found -> None
let get_metadatas t = t.metadata

let log t i =
  Queue.add ((Unix.localtime (Unix.time ())),i) t.log

let on_air t =
  let date = Unix.localtime (Unix.time ()) in
    set_metadata t "on_air" "true" ;
    set_metadata t "on_air_date" (pretty_date date) ;
    log t "Currently on air."

let get_log t = t.log

let string_of_indicator i =
  "[" ^ (String.concat "][" i) ^ "]"

exception No_indicator

let peek_indicator t =
  match t.indicator with
    | [] -> raise No_indicator
    | i::t -> i
let pop_indicator t =
  let i = peek_indicator t in
    t.indicator <- List.tl t.indicator ;
    i
let push_indicator t l =
  if l <> [] then
    ( log t (Printf.sprintf "Pushed [%S;...]." (List.hd l)) ;
      t.indicator <- l@t.indicator )

let is_ready t =
  match t.indicator with
    | hd::tl when Sys.file_exists hd -> true
    | _ -> false

let get_filename t =
  if is_ready t then
    Some (List.hd t.indicator)
  else
    None


(** Request global management.
  Access to the global table is thread-safe, unlike access to a request.
  In some cases (trace) there could be a problem : TODO ? *)

let requests = Hashtbl.create Configure.requests_table_size
let max = Configure.requests_max_id
let lock = Tutils.Mutex.create "Request.lock"

(** next must be called with lock locked. *)
exception Found of int
let next =
  let current = ref 0 in
    fun () ->
      try
	for i = 1 to max do
	  current := (!current+1) mod max ;
	  ( try
	      let t = Hashtbl.find requests !current in
		if t.destroyed then
		  raise (Found !current)
	    with
	      | Not_found -> raise (Found !current) )
	done ;
	(-1)
      with
	| Found r -> r

let alive_requests () =
  Mutex.lock lock ;
  let l = Hashtbl.fold (fun k v l -> if not (v.destroyed) then k::l else l)
	    requests [] in
    Mutex.unlock lock ;
    l

let is_on_air t =
  Some "true" = get_metadata t "on_air"

let on_air_requests () =
  Mutex.lock lock ;
  let l = Hashtbl.fold (fun k v l -> if is_on_air v then k::l else l)
	    requests [] in
    Mutex.unlock lock ;
    l

let resolving t =
  Some "true" = get_metadata t "resolving"

let resolving_requests () =
  Mutex.lock lock ;
  let l = Hashtbl.fold (fun k v l -> if resolving v then k::l else l)
	    requests [] in
    Mutex.unlock lock ;
    l

let create ?audio u =
  Mutex.lock lock ;
  let rid = next () in
    if rid = -1 then
      ( Mutex.unlock lock ;
	Log.log ~label:"request" 3 "No available RID !" ;
	None )
    else
      let t = {
	id = rid ;
	audio = (match audio with None -> true | Some b -> b) ;
	destroyed = false ;
	log = Queue.create () ;
	metadata = Hashtbl.create 10 ;
	indicator = [u] }
      in
	Hashtbl.add t.metadata "uri" u ;
	Hashtbl.add t.metadata "rid" (string_of_int t.id) ;
	Hashtbl.replace requests t.id t ;
	Mutex.unlock lock ;
	Some t

let destroy ?force t =
  assert (not t.destroyed) ;
  set_metadata t "on_air" "false" ;
  if force = Some true ||
    Some "true" <> get_metadata t "persistent"
  then
    begin
      t.destroyed <- true ;
      log t "Request finished." ;
      if Some "true" = get_metadata t "temporary" then
	match get_filename t with
	  | Some f -> Unix.unlink f
	  | None -> ()
    end

let get_id t = t.id
let from_id id =
  Mutex.lock lock ;
  try
    let r = Hashtbl.find requests id in
      Mutex.unlock lock ;
      Some r
  with
    | Not_found -> Mutex.unlock lock ; None


(** Plugins registration. *)

type protocol = {
  resolve : string -> t -> float -> unit ;
  static : bool ;
}

let protocols_doc =
  "Methods to get a file. They are the first part of URIs: 'protocol:args'."
let mresolvers_doc =
  "Methods to extract metadatas from a file."

let protocols = Plug.create ~doc:protocols_doc ~insensitive:true "protocols"
let mresolvers = Plug.create ~doc:mresolvers_doc ~insensitive:true "mresolvers"

(** Resolving engine. *)

type resolve_flag =
  | Resolved
  | Failed
  | Timeout

exception ExnTimeout

let check_fill =
  let ab = Mixer.Buffer.create () in
  let lock = Tutils.Mutex.create "Request.check_fill.lock" in
    fun d ->
      Mutex.lock lock ;
      ignore (d.Decoder.fill ab) ;
      let r = Mixer.Buffer.already ab = 0 in
	Mixer.Buffer.free ab ;
	Mutex.unlock lock ;
	r

let check_audio t =
  if t.audio then
    begin
      log t "Checking that file is audio..." ;
      match get_filename t with
	| Some f ->
	    ( match Decoder.get f with
		| None ->
		    log t "Unable to initiate decoding of that file!" ;
		    false
		| Some d ->
		    if check_fill d then
		      ( log t "Unable to extract audio from that file!" ;
			d.Decoder.close () ;
			false )
		    else
		      ( log t "OK, this is an audio file." ;
			d.Decoder.close () ;
			true ) )
	| None -> assert false
    end
  else
    true

let resolve t timeout =
  Hashtbl.add t.metadata "resolving" "true" ;
  let maxtime = (Unix.time ()) +. timeout in
  let resolve_step () =
    let i = pop_indicator t in
    let ui = home_unrelate i in
      if Sys.file_exists ui then
	let _ = log t (Printf.sprintf "Resolved => local file %S." ui) in
	  push_indicator t [ui]
      else
	let proto,arg = parse_uri i in
	  match protocols#get proto with
	    | Some handler ->
		log t (Printf.sprintf "Resolving %S ..." i) ;
		log t (Printf.sprintf "Timeout is %f sec." timeout) ;
		handler.resolve arg t maxtime ;
	    | None -> log t "Unknown protocol !"
  in
  let resolved_file =
    try
      while not (is_ready t) do
	let timeleft = maxtime -. (Unix.time ()) in
	  if timeleft > 0. then
	    resolve_step ()
	  else
	    ( log t "Global timeout." ; raise ExnTimeout )
      done ;
      Resolved
    with
      | ExnTimeout -> Timeout
      | No_indicator -> log t "Every possibility failed!" ; Failed
  in
  let r =
    begin
      let excess = (Unix.time ()) -. maxtime in
	if excess > 0. then
	  Dtools.Log.log ~label:"request"
	    2 (Printf.sprintf "Time limit exceeded by %f secs!" excess)
    end ;
    if resolved_file = Resolved then
      if check_audio t then
	begin
	  log t "Filling in metadatas ..." ;
	  match get_filename t with
	    | Some fn -> (
		match mresolvers#get (get_extension fn) with
		  | Some resolver ->
		      let ans = resolver fn in
			List.iter
			  (fun (k,v) ->
			     set_metadata t (String.lowercase k) (cleanup v))
			  ans ;
			log t "Done." ;
			Resolved
		  | None ->
		      log t "Failed. However, that's not a fatal error." ;
		      Resolved )
	    | None -> assert false
	end
      else
	Failed
    else
      resolved_file
  in
    Hashtbl.remove t.metadata "resolving" ;
    r

let is_static s =
  if Sys.file_exists (home_unrelate s) 
  then Some true
  else
    let proto,arg = parse_uri s in
      match protocols#get proto with
	| Some handler -> Some handler.static
	| None -> None

let clean () =
  Hashtbl.iter
    (fun k r -> if not r.destroyed then destroy ~force:true r)
    requests
