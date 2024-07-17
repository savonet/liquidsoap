(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

type source = User | Lastfm

let log = Dtools.Log.make ["liqfm"]

exception Duration

(* The list of waiting submitions *)
let submissions = ref []
(* A mutex to manage thread concurrency *)
let submit_m = Mutex.create ()
(* A reference to the task *)
let task = ref None

let client = { client = "lsp"; version = "0.1" }

let init () =
 let reason = log#f 3 "Lastfm Submission failed: %s" in
 (* Define a new task *)
 let rec do_submit () =
   try
    let songs = ref [] in
    (* This function checks that the submission is valid *)
    let song (user,password,source,m) =
      let login = { user = user ; password = password } in
      let f = fun x -> try Hashtbl.find m x with Not_found -> "" in
      let artist,track = f "artist",f "title" in
      log#f 4 "Submiting %s -- %s to lastfm" artist track ;
      try
        let duration =
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
        in
        let song = { artist = artist ; track = track ;
                     time = Some time  ;
                     source = Some source; rating = None ;
                     length = Some duration ;
                     album = Some (f "album") ; tracknumber = None ;
                     musicbrainzid = None ; trackauth = trackauth }
        in
        check_song song Submit ;
        songs := (login,song) :: !songs
      with
        | Duration ->
            log#f 4 "could not submit track %s -- %s, no duration available"
              artist track
        | Error e ->
            log#f 4 "could not submit track %s -- %s, %s"
              artist track (string_of_error e)
        | e ->
            log#f 4 "could not submit track %s -- %s: unknown error %s"
              artist track (Printexc.to_string e)
     in
     Mutex.lock submit_m;
     List.iter song !submissions ;
     submissions := [];
     Mutex.unlock submit_m;
     let submit = Hashtbl.create 10 in
     let filter (c,m) =
       try
         let v = Hashtbl.find submit c in
         Hashtbl.replace submit c (m::v)
       with
         | Not_found -> Hashtbl.add submit c [m]
     in
     List.iter filter !songs ;
     let f login songs =
       try
         ignore(Lastfm.Audioscrobbler.do_submit client login songs)
       with
         | Lastfm.Audioscrobbler.Error e ->
             reason (Lastfm.Audioscrobbler.string_of_error e)
         | Lastfm.Radio.Error e ->
             reason (Lastfm.Radio.string_of_error e)
     in
     Hashtbl.iter f submit
   with
     | e -> reason (Printexc.to_string e)
   in
   (* Add and wake the task *)
   let t =
     Duppy.Async.add ~priority:Tutils.Blocking Tutils.scheduler do_submit
   in
   task := Some t;
   Duppy.Async.wake_up t

let submit (user,password) source metadatas =
 Mutex.lock submit_m;
 let metadatas = List.map (fun x -> (user,password,source,x)) metadatas in
 submissions := !submissions @ metadatas;
 Mutex.unlock submit_m;
 match !task with
   | None -> init ()
   | Some t -> Duppy.Async.wake_up t
