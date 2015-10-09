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

let log = Dtools.Log.make ["liqfm"]

(* A custom implementation of HTTP 
 * requests. *)
module Liq_http =
 struct 
  type request = Get | Post of string

  exception Http of string

  let exc_of_exc = 
    function
      | Http s -> Http s
      | e -> Http (Printexc.to_string e)

  (* This in unused for now.. *)
  let default_timeout = ref 5.
  let request ?timeout ?headers ?port 
              ~host ~url ~request () =
  try
    let log = log#f 4 "%s" in
    let timeout = 
      match timeout with
        | None -> !default_timeout
        | Some t -> t
    in
    let request = 
      match request with
        | Get -> Http.Get
        | Post s -> Http.Post s
    in
    let (x,code,y),_,data = 
          Http.full_request ?headers ?port ~log
                            ~timeout ~host ~url 
                            ~request ()
    in
    if code <> 200 then 
      raise (Http (Printf.sprintf "Http request failed: %s %i %s" x code y));
    data
  with
    | e -> raise (exc_of_exc e)
 end

module Audioscrobbler = Lastfm_generic.Audioscrobbler_generic(Liq_http)

let error_translator =
  function
    | Audioscrobbler.Error x ->
        Some (Printf.sprintf "Audioscrobbler error: %s" 
               (Audioscrobbler.string_of_error x))
    | _ -> None

let () = Utils.register_error_translator error_translator

open Lastfm_generic
open Audioscrobbler

type source = User | Lastfm | Broadcast | Recommendation | Unknown

type submission = NowPlaying | Played

type task =
  { task : Duppy.Async.t ;
    submit_m : Mutex.t ;
    submissions : (string * string * source * submission *
          bool * (string, string) Hashtbl.t) Queue.t ;
  }

let log = Dtools.Log.make ["audioscrobbler"]

exception Duration

let client = { client = "lsp"; version = "0.1" }

let init host =
 (* The list of waiting submitions *)
 let submissions = Queue.create () in
 (* A mutex to manage thread concurrency *)
 let submit_m = Mutex.create () in
 let reason = log#f 3 "Lastfm Submission failed: %s" in
 (* Define a new task *)
 let do_submit () =
   try
    (* This function checks that the submission is valid *)
    let song songs (user,password,(source:source),stype,length,m) =
      let login = { user = user ; password = password } in
      let f = fun x -> try Hashtbl.find m x with Not_found -> "" in
      let artist,track = f "artist",f "title" in
      let s = 
        match stype with
          | Played -> "submit"
          | NowPlaying -> "nowplaying"
      in
      let (h,p) = host in
      log#f 4 "Submiting %s -- %s with mode: %s to %s:%i" 
         artist track s h p;
      try
        let duration () =
          try
            float_of_string (Hashtbl.find m "duration")
          with
            | Not_found | Failure "float_of_string" ->
               try
                 let rid = int_of_string (Hashtbl.find m "rid") in
                 let request = Request.from_id rid in
                   match request with
                     | Some s ->
                         begin
                           match Request.get_filename s with
                         | Some file -> Request.duration file
                         | None -> raise Not_found
                         end
                     | None -> raise Not_found
               with
                 | Not_found -> raise Duration
                 | Failure "int_of_string" ->
                     log#f 2 "Metadata 'rid' is not associated to an integer!" ;
                     raise Duration
        in
        let duration = 
          if length then
            try
              Some (duration ())
            with
              | Duration -> if source = User then raise Duration else None
          else
            if source <> User then None else raise Duration
        in
        let time = Unix.time () in
        let trackauth =
          (* Only when source is lasftm *)
          match source with
            | Lastfm -> Some (f "lastfm:trackauth")
            | _ -> None
        in
        let source = match source with
                       | User -> Audioscrobbler.User
                       | Lastfm -> Audioscrobbler.Lastfm
                       | Broadcast -> Audioscrobbler.Broadcast
                       | Recommendation -> Audioscrobbler.Recommendation
                       | Unknown -> Audioscrobbler.Unknown
        in
        let song = { artist = artist ; track = track ;
                     time = Some time  ;
                     source = Some source; rating = None ;
                     length = duration ;
                     album = Some (f "album") ; tracknumber = None ;
                     musicbrainzid = None ; trackauth = trackauth }
        in
        check_song song Submit ;
        (login,stype,song) :: songs
      with
        | Duration ->
            log#f 4 "could not submit track %s -- %s, no duration available"
              artist track ;
            songs
        | Error e ->
            log#f 4 "could not submit track %s -- %s, %s"
              artist track (string_of_error e) ;
            songs
        | e ->
            log#f 4 "could not submit track %s -- %s: unknown error %s"
              artist track (Printexc.to_string e) ;
            songs
     in
     Mutex.lock submit_m;
     let songs = Queue.fold song [] submissions in
     Queue.clear submissions ;
     Mutex.unlock submit_m;
     let submit = Hashtbl.create 10 in
     let filter (c,t,m) =
       try
         let v = Hashtbl.find submit (c,t) in
         Hashtbl.replace submit (c,t) (m::v)
       with
         | Not_found -> Hashtbl.add submit (c,t) [m]
     in
     List.iter filter songs ;
     let f (login,(stype:submission)) songs =
       try
         match stype with
           | NowPlaying -> 
               List.iter (fun song -> 
                    Audioscrobbler.do_np ~host client login song)
                    songs
           | Played ->
              ignore(Audioscrobbler.do_submit ~host client login songs)
       with
         | Audioscrobbler.Error e ->
             reason (Audioscrobbler.string_of_error e)
     in
     Hashtbl.iter f submit ;
     (-1.)
   with
     | e -> reason (Printexc.to_string e); (-1.)
   in
   let task = 
     Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler do_submit
   in
   { task = task ;
     submit_m = submit_m ;
     submissions = submissions }

let submit (user,password) task length source stype songs =
 let songs = List.map (fun x -> (user,password,source,stype,length,x)) songs in
 Mutex.lock task.submit_m;
 List.iter (fun x -> Queue.add x task.submissions) songs ;
 Mutex.unlock task.submit_m;
 Duppy.Async.wake_up task.task
