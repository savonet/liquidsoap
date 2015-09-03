(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2015 Savonet team

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
 *                   (only defined for Normal and Randomize modes);
 * Watch: the playlist is reloaded whenever it is changed.
 *)
type reload_mode =
  | Never
  | Every_N_rounds of int
  | Every_N_seconds of float
  | Watch

let is_dir f =
  try
    (Unix.stat f).Unix.st_kind = Unix.S_DIR
  with
    | _ -> false

let rec list_files (log : Log.t) dir =
  let dirs, files =
    List.partition is_dir
      (List.map
         (fun s -> dir ^ "/" ^ s)
         (Array.to_list
            (
              try
                Sys.readdir dir
              with
                | Sys_error _ ->
                    log#f 3 "Could not read directory %s" dir ;
                    [||]
            )
         )
      )
  in
  let files = List.sort compare files in
    files@(List.concat (List.map (fun d -> list_files log d) dirs))

(** The [timeout] (and [mime]) parameters apply to the playlist request,
  * i.e. the playlist_uri, not the media files from the playlist. *)
class virtual vplaylist ~mime ~reload ~random ~timeout ~prefix playlist_uri =
object (self)

  method virtual register_command : descr:string ->
                                    ?usage:string -> string ->
                                    (string->string) -> unit

  val mutable virtual ns_kind : string

  initializer
    ns_kind <- "playlist";
    self#register_command "reload"
      ~descr:"Reload the playlist, unless already being loaded."
      (fun _ -> self#reload_playlist `Other ; "OK") ;
    self#register_command "uri"
               ~descr:"Print playlist URI if called without an argument, \
                       otherwise set a new one and load it."
               ~usage:"uri [<URI>]"
      (fun s ->
         if s = "" then
           playlist_uri
         else
           (self#reload_playlist ~uri:s `Other ; "OK")) ;
    self#register_command "next"
      ~descr:"Return up to 10 next URIs to be played."
      (* We cannot return request IDs because we create requests at the last
       * moment. For those requests already created by the Request_source
       * parent class, we also display the status. *)
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


  method private virtual is_valid : string -> bool
  method private virtual check_next : Request.t -> bool
  method virtual stype : Source.source_t
  method virtual id : string
  method virtual set_id : ?definitive:bool -> string -> unit
  method virtual copy_queue : Request.t list
  method virtual create_request :
    ?metadata:((string*string) list) ->
    ?persistent:bool ->
    ?indicators:(Request.indicator list) -> string ->
    Request.t
  method virtual log : Dtools.Log.t
  method virtual private expire : (Request.t -> bool) -> unit

  (** How to get the playlist. *)
  val mutable playlist_uri = playlist_uri

  (** Current playlist, containing the same files, but possibly shuffled. *)
  val playlist = ref [| |]

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

  (** Load or reload playlist and update data.
    * [reload] should be [`No] on the first load, [`Round] for round-based reloads
    * and [`Other] otherwise (i.e., manual, time and watch-based reloads).
    * For the first load, playlist resolution failures won't result in
    * emptying the current playlist.
    * For round-based reloads, already loaded files won't be expired. *)
  method load_playlist ?(uri=playlist_uri) (reload : [`No|`Round|`Other]) =
    let _playlist =
      let read_playlist filename =
        if is_dir filename then begin
          self#log#f 3 "Playlist is a directory." ;
          list_files self#log filename
        end else
          try
             (* TODO This is quite inefficient:
              *   reading the whole file as a string before parsing it... *)
            let content = Utils.read_all filename in
            let pwd =
              if Http.is_url uri then
                Http.dirname uri
              else
                Filename.dirname filename
            in
              let (format,playlist) =
                match mime with
                  | "" ->
                      self#log#f 3
                        "No mime type specified, trying autodetection." ;
                      Playlist_parser.search_valid ~pwd content
                  | x ->
                      begin match Playlist_parser.parsers#get x with
                        | Some plugin ->
                            (x,plugin.Playlist_parser.parser content)
                        | None ->
                            self#log#f 3
                              "Unknown mime type, trying autodetection." ;
                            Playlist_parser.search_valid content
                      end
              in
                self#log#f 3 "Playlist treated as format %s" format  ;
                List.map (fun (l, x) ->
                  if l = [] then x else
                    Printf.sprintf "annotate:%s:%s"
                      (String.concat "," (List.map
                        (fun (l,v) -> Printf.sprintf "%s=%S" l v) l))
                      x) playlist
          with
            | e ->
                self#log#f 3
                  "Could not parse playlist: %s" (Printexc.to_string e) ;
                []
      in
      let req =
        self#log#f 3 "Loading playlist..." ;
        Request.create_raw uri
      in
        match Request.resolve req timeout with
          | Request.Resolved ->
              let l =
                read_playlist (Utils.get_some (Request.get_filename req))
              in
              Request.destroy req ;
              l
          | e ->
              let reason =
                match e with
                  | Request.Timeout -> "Timeout"
                  | Request.Failed -> "Failed"
                  | Request.Resolved -> assert false
              in
              self#log#f 2
                "%s when resolving playlist URI %S!"
                reason uri ;
              Request.destroy req ;
              []
    in
    (* Remove invalid entries, add prefix to all requests. *)
    let _playlist =
      List.map
        (Printf.sprintf "%s%s" prefix)
        (List.filter self#is_valid _playlist)
    in
      (* TODO distinguish error and empty if fallible *)
      if _playlist = [] && reload <> `No then
        self#log#f 3 "Got an empty list: keeping the old one."
      else begin
        (* Don't worry if a reload fails,
         * otherwise, the source type must be aware of the failure *)
        assert (not (self#stype = Infallible && _playlist = [])) ;
        let array_playlist = Array.of_list _playlist in
        if random = Randomize then Utils.randomize array_playlist ;

        Mutex.lock mylock ;
        playlist := array_playlist ;
        playlist_uri <- uri ;
        (* Even in Normal playing mode (no randomization) it doesn't
         * necessarily make sense to keep the old index. It would if the
         * list wasn't modified, or was simply extended, but that's hard
         * to detect for sure. So we always reset the index and mark
         * old requests as expired. Users who don't want that we'll have
         * to avoid useless reloading. *)
        index_played <- -1 ;
        if reload <> `Round then self#expire (fun _ -> true) ;
        Mutex.unlock mylock ;

        self#log#f 3
          "Successfully loaded a playlist of %d tracks."
          (Array.length !playlist)
      end

  (** Schedule playlist reloading as a duppy task.
    * No guarantee is provided concerning the relative execution
    * order of multiple reloading tasks.
    * Several calls to #load_playlist may even overlap, which
    * means that an automatic reload triggered in the middle of
    * the user-requested loading of a new playlist URI may end up
    * restoring the old URI.
    * This is bad but we prefer to not block the Duppy scheduler
    * with mutexes, waiting for a fully satisfying solution using
    * Duppy mutexes -- and perhaps support for blocking server commands
    * enabling a synchronous reload command. *)
  method reload_playlist ?uri (reload_kind : [`Round|`Other]) =
    Duppy.Task.add Tutils.scheduler
      { Duppy.Task.
          priority = Tutils.Maybe_blocking ;
          events   = [`Delay 0.] ;
          handler  = (fun _ ->
            self#load_playlist ?uri (reload_kind:>[`No|`Other|`Round]) ;
            []) }

  (** Schedule reloading if necessary.
    * The reload parameters round_c/reload_t are updated
    * immediately to avoid most multiple reloads.
    * They can still occur, eg. if actual reloading time is longer than
    * the reloading period set by the user. The current code is just
    * a cheap and reasonably good solution until we implement a fully
    * satisfying system (cf. #reload_playlist's comment) that not
    * only prevents multiple automatic reloads but completely gets
    * rid of reload overlaps, which requires more duppy synchronization. *)
  method private may_autoreload round_done =
    assert (Tutils.seems_locked mylock) ;
    match reload with
      | Never | Watch -> ()
      | Every_N_seconds n ->
          if Unix.time () -. reload_t > n then begin
            reload_t <- Unix.time () ;
            self#reload_playlist `Other
          end
      | Every_N_rounds n ->
          if round_done then round_c <- round_c - 1 ;
          if round_c <= 0 then begin
            round_c <- n ;
            self#reload_playlist `Round
          end

  method get_next_request : Request.t option =
    Mutex.lock mylock ;
    let rec get_uri () =
      (* URI to be played together with
       * position in playlist at the time of selection. *)
      let pos,uri =
        match random with
          | Randomize ->
              index_played <- index_played + 1 ;
              let pos = index_played in
              let uri = !playlist.(index_played) in
              let round =
                if index_played < Array.length !playlist - 1 then
                  false
                else begin
                  index_played <- -1 ;
                  Utils.randomize !playlist ;
                  true
                end
              in
                self#may_autoreload round ;
                pos,uri
          | Random ->
              index_played <- Random.int (Array.length !playlist) ;
              self#may_autoreload false ;
              index_played,!playlist.(index_played)
          | Normal ->
              index_played <- index_played + 1 ;
              let pos = index_played in
              let uri = !playlist.(index_played) in
              let round =
                if index_played < Array.length !playlist - 1 then
                  false
                else begin
                  index_played <- -1 ;
                  true
                end
              in
                self#may_autoreload round ;
                pos,uri
      in
      let metadata =
        [ "playlist_position", string_of_int pos ;
          "playlist_length", string_of_int (Array.length !playlist) ]
      in
      let r = self#create_request ~metadata uri in
      let id = Request.get_id r in
        if self#check_next r then begin
          Mutex.unlock mylock ;
          Some r
        end else begin
          self#log#f 3 "Request (RID %d) rejected by check_next!" id ;
          Request.destroy r ;
          get_uri ()
        end
    in
      if !playlist = [||] then begin
        self#may_autoreload true ;
        Mutex.unlock mylock ;
        None
      end else get_uri ()

  method playlist_wake_up =
    let base_name =
      let x = Filename.basename playlist_uri in
      match x with
        | "." ->
            begin
              match List.rev (Pcre.split ~pat:"/" playlist_uri) with
                | e :: _ -> e
                | [] -> Filename.dirname playlist_uri
            end
        | _ -> x
    in
    let id =
      match base_name with
        | "." -> (* Definitly avoid this *)
            Printf.sprintf "playlist-%s" playlist_uri
        | _ -> base_name
    in
    self#set_id ~definitive:false id ;
    (* Proceed to initial playlist loading.
     * It is important that this is done before initialization
     * of the queued request source, which could trigger a concurrent
     * reload. We initialize reload parameter to avoid an automatic
     * reload immediately after this one.
     * It would be possible to not force initial loading like we do,
     * and let the rest of the system to trigger it when needed.
     * However, in case of playlist.safe() this delays the critical
     * check for playlist safety, making things messy. *)
    Mutex.lock mylock ;
    begin match reload with
      | Never | Watch -> ()
      | Every_N_rounds n -> round_c <- n
      | Every_N_seconds _ -> reload_t <- Unix.time ()
    end ;
    Mutex.unlock mylock ;
    self#load_playlist `No

  (** Give the next [n] URIs, if guessing is easy. *)
  method get_next = Tutils.mutexify mylock (fun n ->
    match random with
    | Normal ->
        Array.init
          n
          (fun i -> !playlist.((index_played+i+1) mod (Array.length !playlist)))
    | Randomize ->
        Array.init
          (min n (Array.length !playlist - index_played - 1))
          (fun i -> !playlist.(index_played+i+1))
    | Random -> [||])

end

(** Standard playlist, with a queue. *)
class playlist ~kind
  ~mime ~reload ~random ~check_next
  ~length ~default_duration ~timeout ~prefix ~conservative
  uri =
object (self)

  (* Some day it might be useful to set distinct timeout parameters
   * for the playlist and media requests... or maybe not. *)
  inherit vplaylist ~mime ~reload ~random ~timeout ~prefix uri as pl
  inherit Request_source.queued ~kind ~name:"playlist"
            ~length ~default_duration ~timeout ~conservative () as super

  method load_playlist ?uri is_reload =
    pl#load_playlist ?uri is_reload ;
    super#notify_new_request

  method wake_up activation =
    pl#playlist_wake_up ;
    super#wake_up activation

  (** Assume that every URI is valid, it will be checked on queuing. *)
  method private is_valid _ = true

  method get_ready ?dynamic sl =
    super#get_ready ?dynamic sl;
    let watch = !Configure.file_watcher in
    if reload = Watch then
      self#on_shutdown
        (watch [`Modify] (Utils.home_unrelate playlist_uri)
          (fun () -> self#reload_playlist ~uri:playlist_uri `Other))

  method private check_next r =
    Lang.to_bool
      (Lang.apply ~t:Lang.bool_t check_next ["",Lang.request r])

end

(** Safe playlist, without queue and playing only local files,
  * which never fails. *)
class safe_playlist ~kind
  ~mime ~reload ~random ~prefix local_playlist =
object (self)

  inherit vplaylist
    ~mime ~reload ~random ~timeout:10. ~prefix local_playlist as pl
  inherit Request_source.unqueued ~kind ~name:"playlist.safe" as super

  method wake_up =
    pl#playlist_wake_up ;
    super#wake_up

  (** We check that the lines are valid local files,
    * thus, we can assume that the source is infallible. *)
  method private is_valid uri =
    Sys.file_exists uri &&
    let r = Request.create ~kind uri in
    let check = Request.resolve r 0. = Request.Resolved in
      Request.destroy r ;
      check

  method private check_next _ = true

  method stype = Infallible

  method load_playlist ?uri reload =
    pl#load_playlist ?uri reload ;
    if Array.length !playlist = 0
    then failwith "Empty default playlist !" ;

  (** Directly play the files of the playlist. *)
  method get_next_file = pl#get_next_request

  (** Nothing queued, hence nothing to expire. *)
  method private expire _ = ()

  method get_ready ?dynamic sl =
    super#get_ready ?dynamic sl;
    let watch = !Configure.file_watcher in
    if reload = Watch then
      self#on_shutdown (watch [`Modify] (Utils.home_unrelate playlist_uri)
        (fun () -> self#reload_playlist ~uri:playlist_uri `Other))
end


let () =
  let proto =
    [ "mode",
      Lang.string_t,
      (Some (Lang.string "randomize")),
      Some "Play the files in the playlist either in the order (\"normal\" \
            mode), or shuffle the playlist each time it is loaded, \
            and play it in this order for a whole round (\"randomize\" mode), \
            or pick a random file in the playlist each time \
            (\"random\" mode)." ;

      "reload",
      Lang.int_t,
      Some (Lang.int 0),
      Some "Amount of time (in seconds or rounds), when applicable, before which \
            the playlist is reloaded; 0 means never." ;

      "reload_mode",
      Lang.string_t,
      Some (Lang.string "seconds"),
      Some "Unit of the reload parameter, either 'rounds', 'seconds' or \
            'watch' (reload the file whenever it is changed)." ;

      "mime_type",
      Lang.string_t,
      Some (Lang.string ""),
      Some "Default MIME type for the playlist. \
            Empty string means automatic detection." ;

      "prefix",
      Lang.string_t,
      Some (Lang.string ""),
      Some "Add a constant prefix to all requests. \
            Useful for passing extra information using annotate, \
            or for resolution through a particular protocol, such \
            as replaygain." ;

      "",
      Lang.string_t,
      None,
      Some "URI where to find the playlist." ]
  in
  let reload_of i s =
    let arg = Lang.to_int i in
    let ss = Lang.to_string s in
      if ss = "watch" then Watch
      else
        (
          if arg < 0 then raise (Lang.Invalid_value (i,"must be positive")) ;
          if arg = 0 then Never else
            begin match ss with
            | "rounds"  -> Every_N_rounds arg
            | "seconds" -> Every_N_seconds (float_of_int arg)
            | _ ->
              raise (Lang.Invalid_value
                       (s,"valid values are 'rounds', 'seconds' and 'watch'"))
            end
        )
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
  let check_next k =
    "check_next",
    Lang.fun_t [false,"",Lang.request_t k] Lang.bool_t,
    Some (Lang.val_fun
            ["","r",Lang.request_t k,None]
            ~ret_t:Lang.bool_t
            (fun _ _ -> Lang.bool true)),
    Some "Function used to filter next tracks. A candidate \
          track is only validated if the function returns true on it. \
          The function is called before resolution, hence metadata will \
          only be available for requests corresponding to local files. \
          This is typically used to avoid repetitions, but be careful: \
          if the function rejects all attempts, the playlist will enter \
          into a consuming loop and stop playing anything."
  in

  let fmt = Lang.univ_t 1 in

    Lang.add_operator "playlist"
      ~category:Lang.Input
      ~descr:"Loop on a playlist of URIs."
      (Request_source.queued_proto @ check_next fmt :: proto)
      ~kind:(Lang.Unconstrained fmt)
      (fun params kind ->
         let reload,random,mime,uri,prefix =
           let e v = List.assoc v params in
             (reload_of (e "reload") (e "reload_mode")),
             (random_of (e "mode")),
             (Lang.to_string (e "mime_type")),
             (Lang.to_string (e "")),
             (Lang.to_string (e "prefix"))
         in
         let check_next = List.assoc "check_next" params in
         let length,default_duration,timeout,conservative =
                Request_source.extract_queued_params params
         in
           ((new playlist ~kind ~mime ~reload ~random
                          ~length ~default_duration ~prefix ~timeout
                          ~check_next ~conservative uri):>source)) ;

    Lang.add_operator "playlist.safe"
      ~category:Lang.Input
      ~descr:"Loop on a playlist of local files, \
              and never fail. In order to do so, it has to check \
              every file at the loading, so the streamer startup may take \
              a few seconds. To avoid this, use a standard playlist, \
              and put only a few local files in a default safe_playlist \
              in order to ensure the liveness of the streamer."
      proto
      ~kind:(Lang.Unconstrained (Lang.univ_t 1))
      (fun params kind ->
          let reload,random,mime,uri,prefix =
           let e v = List.assoc v params in
             (reload_of (e "reload") (e "reload_mode")),
             (random_of (e "mode")),
             (Lang.to_string (e "mime_type")),
             (Lang.to_string (e "")),
             (Lang.to_string (e "prefix"))
         in
           ((new safe_playlist ~kind ~mime ~reload ~prefix
                               ~random uri):>source))
