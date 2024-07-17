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

(** Streaming a playlist *)

open Source
open Dtools

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
  let a = Array.make n "" in
    ignore (List.fold_left
	      (fun i e ->
		 a.(i) <- e ; i-1) (n-1) l) ;
    a

class virtual vplaylist reload random playlist_uri =
object (self)

  method virtual is_valid : string -> bool
  method virtual stype : Source.source_t
  method virtual id : string
  method virtual set_id : ?definitive:bool -> string -> unit
  method virtual copy_queue : Request.t list
  method virtual create_request :
    ?metadata:((string*string) list) -> 
    ?audio:bool -> ?persistent:bool ->
    ?indicators:(Request.indicator list) -> string ->
    Request.t option
  method virtual log : int -> string -> unit

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
  val mylock = Mutex.create ()

  (** Lock for avoiding multiple reloads at the same time. *)
  val reloading = Mutex.create ()

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

  method load_playlist ?(uri=playlist_uri) reload =
    let _playlist =
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
          let rec read_playlist n acc =
            let line = 
              try (* Looks weird ? Just a tail-rec trick. *)
                let l = input_line in_chan in
                  if l = "" then "#" else l
              with
                | End_of_file -> ""
            in
              if line = "" then acc else
                let acc =
                  if line.[0] = '#' then acc else
                    if self#is_valid line then line::acc else begin
                      self#log 2 (Log.f "Invalid line %d" n) ;
                      acc
                    end
                in
                  read_playlist (n+1) acc
          in
          self#log 3 "Loading playlist..." ;
          let p = read_playlist 1 [] in
            close_in in_chan ;
            Request.destroy req ;
            p
        else begin
          Request.destroy req ;
          []
        end
    in
      if not (_playlist = [] && reload) then begin
        (* Don't worry if a reload fails,
         * otherwise, the source type must be aware of the failure *)
        Mutex.lock mylock ;
        assert (not (self#stype = Infallible && _playlist = [])) ;
        playlist := rev_array_of_list _playlist ;
        playlist_uri <- uri ;
        Mutex.unlock mylock
      end

  (* [reloading] avoids two reloadings being prepared at the same time. *)
  method reload_playlist ?new_reload ?new_random ?new_playlist_uri () =
    if Mutex.try_lock reloading then
      Tutils.Task.create
        (fun () ->
          self#reload_playlist_internal
          new_reload new_random new_playlist_uri ;
          Mutex.unlock reloading ;
          Tutils.Task.Finish)

  method reload_playlist_nobg ?new_reload ?new_random ?new_playlist_uri () =
    if Mutex.try_lock reloading then
      ( self#reload_playlist_internal new_reload new_random new_playlist_uri ;
	Mutex.unlock reloading )

  method reload_playlist_internal new_reload new_random new_playlist_uri =

    assert (not (Mutex.try_lock reloading)) ;

    self#load_playlist ?uri:new_playlist_uri true ;

    Mutex.lock mylock ;
    ( match new_random with
	| None -> ()
	| Some m -> random <- m ) ;
    ( match new_reload with
	| None -> ()
	| Some m -> reload <- m ) ;
    if Randomize = random then
      self#randomize_playlist ;
    ( match reload with
	| Never -> ()
	| Every_N_rounds n -> round_c <- n
	| Every_N_seconds n -> reload_t <- Unix.time () ) ;
    Mutex.unlock mylock

  method reload_update round_done =
    (* Must be called by somebody who owns [mylock] *)
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

  val mutable ns = []
  method playlist_wake_up =
    self#set_id ~definitive:false (Filename.basename playlist_uri) ;
    self#reload_playlist_nobg () ;
    if ns = [] then
      ns <- Server.register [self#id] "playlist" ;
    Server.add ~ns "next"
      (* The next command returns up to 10 next URIs to be played.
       * We cannot return request ids cause we create requests at the last
       * moment. We get requests from Request_source.* classes, from which
       * we get a status. *)
      (fun s ->
         let n =
           try int_of_string s with _ -> 10
         in
           Array.fold_left
             (fun s uri -> s^uri^"\n")
             (List.fold_left
                (fun s r ->
                   let get s =
                     match Request.get_metadata r s with
                       | Some s -> s | None -> "?"
                   in
                     (Printf.sprintf "[%s] %s\n"
                        (get "status") (get "initial_uri"))
                     ^ s)
                "" self#copy_queue)
             (self#get_next n))

  (* Give the next [n] URIs, if guessing is easy. *)
  method get_next n =
    Mutex.lock mylock ;
    match random with
    | Normal ->
        let a = Array.make n "" in
          for i = 1 to n do
            a.(i-1) <- !playlist.((index_played+i) mod (Array.length !playlist))
          done ;
          Mutex.unlock mylock ;
          a
    | Randomize ->
        let n = min n (Array.length !playlist)-index_played-1 in
        let a = Array.make n "" in
          for i = 1 to n do
            a.(i-1) <- !playlist.(index_played+i)
          done ;
          Mutex.unlock mylock ;
          a
    | Random -> Mutex.unlock mylock ; [||]

end

(** Standard playlist, with a queue. *)
class playlist reload_meth rand_meth uri length default_duration timeout =
object

  inherit vplaylist reload_meth rand_meth uri as pl
  inherit Request_source.queued ~length ~default_duration ~timeout () as super

  method load_playlist ?(uri=playlist_uri) reload =
    pl#load_playlist ~uri reload ;
    super#notify_new_request

  method wake_up activation =
    super#wake_up activation ;
    pl#playlist_wake_up

  (** Assume that every URI is valid, it will be checked on queuing. *)
  method is_valid file = true

end

(** Safe playlist, without queue and playing only local files,
  which never fails. *)
class safe_playlist reload_meth rand_meth local_playlist = 
object (self)

  inherit vplaylist reload_meth rand_meth local_playlist as pl
  inherit Request_source.unqueued as super

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

  method load_playlist ?uri reload =
    pl#load_playlist ?uri reload ;
    if Array.length !playlist = 0
    then failwith "Empty default playlist !" ;

  (** Directly play the files of the playlist. *)
  method get_next_file = pl#get_next_request

end


let _ =

  let proto =
    [ "mode",
      Lang.string_t,
      (Some (Lang.string "randomize")),
      (Some "normal|random|randomize") ;

      "reload",
      Lang.int_t,
      (Some (Lang.int 0)),
      Some ("Amount of time (in seconds or rounds) before which "^
            "the playlist is reloaded; 0 means never.") ;

      "reload_mode",
      Lang.string_t,
      (Some (Lang.string "seconds")),
      (Some ("rounds|seconds: unit of the 'reload' parameter")) ;

      "",
      Lang.string_t,
      None,
      (Some "URI where to find the playlist") ]
  in
  let reload_of i s =
    let arg = Lang.to_int i in
      if arg<0 then
        raise (Lang.Invalid_value (i,"must be positive")) ;
      if arg = 0 then Never else
      begin match Lang.to_string s with
        | "rounds"  -> Every_N_rounds arg
        | "seconds" -> Every_N_seconds (float_of_int arg)
        | _ ->
            raise (Lang.Invalid_value
                     (s,"valid values are 'rounds' and 'seconds'"))
      end
  in
  let random_of s =
    match Lang.to_string s with
      | "random" -> Random
      | "randomize" -> Randomize
      | "normal" -> Normal
      | _ ->
          raise (Lang.Invalid_value
                   (s,"valid values are 'random', 'randomize' and 'normal'"))
  in

    Lang.add_operator "playlist"
      ~descr:"Loop on a playlist of URIs."
      (Request_source.queued_proto@proto)
      (fun params ->
         let reload,random,uri = 
           let e v = List.assoc v params in
             (reload_of (e "reload") (e "reload_mode")),
             (random_of (e "mode")),
             (Lang.to_string (e ""))
         in
         let l,d,t = Request_source.extract_queued_params params in
           ((new playlist reload random uri l d t):>source)) ;

    Lang.add_operator "playlist.safe"
      ~descr:("Loop on a playlist of local files, "^
              "and never fail. In order to do so, it has to check "^
              "every file at the loading, so the streamer startup may take "^
              "a few seconds. To avoid this, use a standard playlist, "^
              "and put only a few local files in a default safe_playlist "^
              "in order to ensure the liveness of the streamer.")
      proto
      (fun params ->
         let reload,random,uri = 
           let e v = List.assoc v params in
             (reload_of (e "reload") (e "reload_mode")),
             (random_of (e "mode")),
             (Lang.to_string (e ""))
         in
           ((new safe_playlist reload random uri):>source))
