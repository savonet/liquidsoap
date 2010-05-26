(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

open Lastfm
open Lastfm.Audioscrobbler
open Duppy

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

let conf_liqfm =
  Dtools.Conf.void ~p:(Configure.conf#plug "audioscrobbler")
	    "Audioscrobbler configuration."
let conf_timeout =
  Dtools.Conf.float ~p:(conf_liqfm#plug "timeout") ~d:5.
    "Default timeout for HTTP requests."

let client = { client = "lsp"; version = "0.1" }

let init host =
 (* The list of waiting submitions *)
 let submissions = Queue.create () in
 (* A mutex to manage thread concurrency *)
 let submit_m = Mutex.create () in
 Lastfm.Http_ocamlnet.default_timeout := conf_timeout#get ;
 let reason = log#f 3 "Lastfm Submission failed: %s" in
 (* Define a new task *)
 let rec do_submit () =
   try
    (* This function checks that the submission is valid *)
    let song songs (user,password,source,stype,length,m) =
      let login = { user = user ; password = password } in
      let f = fun x -> try Hashtbl.find m x with Not_found -> "" in
      let artist,track = f "artist",f "title" in
      let s = 
        match stype with
          | Played -> ""
          | NowPlaying -> " (nowplaying)"
      in
      let (h,p) = host in
      log#f 4 "Submiting %s -- %s%s to %s:%i" 
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
                       | User -> Lastfm.Audioscrobbler.User
                       | Lastfm -> Lastfm.Audioscrobbler.Lastfm
                       | Broadcast -> Lastfm.Audioscrobbler.Broadcast
                       | Recommendation -> Lastfm.Audioscrobbler.Recommendation
                       | Unknown -> Lastfm.Audioscrobbler.Unknown
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
     let f (login,stype) songs =
       try
         match stype with
           | NowPlaying -> 
               List.iter (fun song -> 
                    Lastfm.Audioscrobbler.do_np ~host client login song)
                    songs
           | Played ->
              ignore(Lastfm.Audioscrobbler.do_submit ~host client login songs)
       with
         | Lastfm.Audioscrobbler.Error e ->
             reason (Lastfm.Audioscrobbler.string_of_error e)
         | Lastfm.Radio.Error e ->
             reason (Lastfm.Radio.string_of_error e)
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
