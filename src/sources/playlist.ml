
(** Streaming a playlist *)

open Types
open Dtools

let log = Log.log ~label:"playlist"
let logl = Log.logl ~label:"playlist"

(* Random: every file is choosed randomly.
 * Randomize: the playlist is shuffled, then read linearly,
 *            and shuffled again when the end is reached, and so on.
 * Normal: the playlist is read normally, and loops. *)
type random_mode = Random | Randomize | Normal


(* Never: never reload the playlist.
 * With the other reloading modes, reloading may be triggered after a file
 * is choosed -- it requires the source to be selected:
 * Every_N_seconds n: the playlist is reloaded every n seconds;
 * Every_N_rounds n: A round is every time the end of playlist is reached
 *                   (only defined for Normal and Randomize modes).
 *)
type reload_mode = Never | Every_N_rounds of int | Every_N_seconds of float

let rev_array_of_list l =
  let n = List.length l in
  let a = Array.make n (Obj.magic 0) in
    ignore (List.fold_left
	      (fun i e ->
		 a.(i) <- e ; i-1) (n-1) l) ;
    a

class virtual vplaylist = fun reload random playlist_uri ->
object (self)

  (** How to get the playlist. *)
  val mutable playlist_uri = playlist_uri

  (** Current playlist, containing the same files, but possibly shuffled.
    The complex type is/was for a possible feature with strider. *)
  val mutable playlist = ref [| |]

  (** Index of the current file. *)
  val mutable index_played = -1

  (** Random mode. *)
  val mutable random = random

  (** Reload mode with reload status in term of rounds and time. *)
  val mutable reload = reload
  val mutable round_c = 0
  val mutable reload_t = 0.

  (** Lock for the previous variables. *)
  val mylock = Tutils.Mutex.create "Playlist.mylock"

  (** Lock for avoiding multiple reloads at the same time. *)
  val reloading = Tutils.Mutex.create "Playlist.reloading"

  (** Randomly exchange files in the playlist.
    Must be called within mylock critical section. *)

  method randomize_playlist =
    assert (not (Mutex.try_lock mylock)) ;
    let permute i j =
      let tmp = !playlist.(i) in
	!playlist.(i) <- !playlist.(j);
	!playlist.(j) <- tmp
    in
    let l = Array.length !playlist in
      for i=0 to l-1 do
	permute i (Random.int l)
      done

  (** (re-)read playlist_file and update datas.
    [reload] should be true except on first load, in that case playlist
    resolution failures won't result in emptying the current playlist.*)

  method load_playlist reload =
    let _playlist =
      let uri = playlist_uri in
      let req = match Request.create ~audio:false uri with
	| None -> assert false
	| Some r -> r
      in
	if Request.Resolved = Request.resolve req 10. then
	  let filename =
	    match Request.get_filename req with
	      | None -> assert false
	      | Some f -> f
	  in

          (* Parsing the playlist *)
	  let in_chan = open_in_bin filename in
	  let rec read_playlist acc =
	    let line = 
	      try (* Looks weird ? Just a tail-rec trick. *)
		let l = input_line in_chan in
		  if l = "" then "#" else l
	      with
		      | End_of_file -> ""
	    in
	      if line = "" then acc else
		read_playlist
		  ( if line.[0] = '#' then acc else
		      if self#is_valid line
		      then line::acc
		      else
			( log 2 (Log.f "Invalid line %S in playlist %S."
				   line uri) ;
			  acc ) )
	  in
	  let p = read_playlist [] in
	    close_in in_chan;
	    Request.destroy req; p
        else
          ( Request.destroy req ; [] )

    in
      if not (_playlist = [] && reload) then
        (* Don't worry if a reload fails,
         * otherwise, the source type must be aware of the failure *)
        let locked = Mutex.try_lock mylock in
          assert (not (self#stype = Infallible && _playlist = [])) ;
          playlist := rev_array_of_list _playlist ;
          if locked then Mutex.unlock mylock

  method reload_playlist ?new_reload ?new_random ?new_playlist_uri () =
    if Mutex.try_lock reloading then
      ignore
        (Tutils.create
           (fun () ->
              self#reload_playlist_internal
                new_reload new_random new_playlist_uri ;
              Mutex.unlock reloading) ()
           "playlist reloading")

  method reload_playlist_nobg ?new_reload ?new_random ?new_playlist_uri () =
    if Mutex.try_lock reloading then
      ( self#reload_playlist_internal new_reload new_random new_playlist_uri ;
	Mutex.unlock reloading )

  method reload_playlist_internal new_reload new_random new_playlist_uri =

    assert (not (Mutex.try_lock reloading)) ;

    Mutex.lock mylock ;
    ( match new_playlist_uri with
	| None -> ()
	| Some d -> playlist_uri <- d ) ;
    ( match new_random with
	| None -> ()
	| Some m -> random <- m ) ;
    ( match new_reload with
	| None -> ()
	| Some m -> reload <- m ) ;

    self#load_playlist true ;

    if Randomize = random then
      self#randomize_playlist ;
    ( match reload with
	| Never -> ()
	| Every_N_rounds n -> round_c <- n
	| Every_N_seconds n -> reload_t <- Unix.time () ) ;
    Mutex.unlock mylock

  method reload_update round_done =
    (* I must be called by somebody who owns [mylock] *)
    assert (not (Mutex.try_lock mylock)) ;
    match reload with
      | Never -> ()
      | Every_N_seconds n ->
	  if Unix.time () -. reload_t > n then
	    self#reload_playlist ()
      | Every_N_rounds n ->
	  if round_done then round_c <- round_c - 1 ;
	  if round_c <= 0 then
	    self#reload_playlist ()

  method get_next_request : Request.t option =
    Mutex.lock mylock ;
    if !playlist = [||] then
      ( self#reload_update true ;
        Mutex.unlock mylock ;
        None )
    else
      let uri =
        match random with
          | Randomize ->
              index_played <- index_played + 1 ;
              let round =
                if index_played >= Array.length !playlist
                then ( index_played <- 0 ;
		       self#randomize_playlist ;
		       true )
		else false
	      in
		self#reload_update round ;
		!playlist.(index_played)
	  | Random ->
	      index_played <- Random.int (Array.length !playlist) ;
	      self#reload_update false ;
	      !playlist.(index_played)
	  | Normal ->
	      index_played <- index_played + 1 ;
	      let round =
                if index_played >= Array.length !playlist
                then ( index_played <- 0 ; true )
                else false
              in
                self#reload_update round ;
		!playlist.(index_played)
      in
	Mutex.unlock mylock ;
	self#create_request uri

  method playlist_wake_up =
    self#reload_playlist_nobg ()

end

(** Standard playlist, with a queue. *)
class playlist reload_meth rand_meth uri = 
object

  inherit vplaylist reload_meth rand_meth uri as pl
  inherit Play_files.queued as super

  method wake_up =
    pl#playlist_wake_up ;
    super#wake_up

  (** Assume that each every URI is valid, it will be checked on queuing. *)
  method is_valid file = true

end

(** Default playlist, without queues and playing only local files,
  which never fails. *)
class default_playlist reload_meth rand_meth local_playlist = 
object (self)

  inherit vplaylist reload_meth rand_meth local_playlist as pl
  inherit Play_files.unqueued as super

  method wake_up =
    pl#playlist_wake_up ;
    super#wake_up

  (** We check that the lines are valid local files,
    * thus, we can assume that the source is infallible. *)
  method is_valid uri =
    Sys.file_exists uri &&
    match Request.create uri with
      | None -> assert false
      | Some r ->
	  let check = Request.resolve r 0. = Request.Resolved in
	    Request.destroy r ;
	    check

  method stype = Infallible

  method load_playlist reload =
    pl#load_playlist reload ;
    if Array.length !playlist = 0
    then failwith "Empty default playlist !" ;

  (** Directly play the files of the playlist. *)
  method get_next_file = pl#get_next_request

end


let _ =

  let prototype =
    ("mode",
     Lang.String_t,
     (Some (Lang.String "randomize")),
     (Some "normal|random|randomize"))::
    ("reload",
     Lang.Int_t,
     (Some (Lang.Int 69)),
     None)::
    ("reload_mode",
     Lang.String_t,
     (Some (Lang.String "never")),
     (Some
        ("never|rounds|seconds: never reload, reload every 'reload' rounds " ^
         "or 'reload' seconds")))::
    ["arg_1",
     Lang.String_t,
     None,
     (Some "URI where to find the playlist")]
  in
  let proto = Lang.internal_of_prototype prototype in
  let reload_of = function
    | (Lang.Int i), (Lang.String s) ->
        if i<=0 then
          raise (Lang.Invalid_value
		   ("reload", "must be strictly positive")) ;
	( match s with
	    | "rounds"  -> Every_N_rounds i
	    | "never"   -> Never
	    | "seconds" -> Every_N_seconds (float_of_int i)
	    | _ ->
		raise (Lang.Invalid_value
			 ("reload_mode",
			  "valid values are 'rounds', 'seconds' and 'never'")))
    | _ -> assert false
  in
  let random_of = function
    | Lang.String s ->
	( match s with
	    | "random" -> Random
	    | "randomize" -> Randomize
	    | "normal" -> Normal
	    | _ ->
		raise (Lang.Invalid_value
			 ("mode", "valid values are 'random', 'randomize' and "
			    ^ "'normal'")) )
    | _ -> assert false
  in

    Lang.operators#register
      ~doc:(Lang.to_doc "Loops on a playlist of URIs." prototype)
      "playlist"
      (fun p ->
	 let p = Lang.check proto p in
	 let reload,random,uri = 
	   (reload_of ((Hashtbl.find p "reload"),
		       (Hashtbl.find p "reload_mode"))),
	   (random_of (Hashtbl.find p "mode")),
	   (Lang.to_string (Hashtbl.find p "arg_1"))
	 in
	   ((new playlist reload random uri):>source)) ;

    Lang.operators#register
      ~doc:(Lang.to_doc
              ("Loops on a playlist of local files, "^
               "and never fails. In order to do so, it has to check "^
               "every file at the loading, so the streamer startup may take "^
               "a few seconds. To avoid this, use a standard playlist, "^
               "and put only a few local files in a default local_playlist "^
               "in order to ensure the liveness of the streamer.") prototype)
      "local_playlist"
      (fun p ->
         let p = Lang.check proto p in
         let reload,random,uri = 
           (reload_of ((Hashtbl.find p "reload"),
                       (Hashtbl.find p "reload_mode"))),
           (random_of (Hashtbl.find p "mode")),
           (Lang.to_string (Hashtbl.find p "arg_1"))
         in
           ((new default_playlist reload random uri):>source))
