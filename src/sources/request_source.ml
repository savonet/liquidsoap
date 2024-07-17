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

open Types
open Dtools

(** Class [unqueued] plays the file given by method [get_next_file]
  * as a request which is ready, i.e. has been resolved.
  * On the top of it we define [queued], which manages a queue of files, feed
  * by resolving in an other thread requests given by [get_next_request]. *)
class virtual unqueued =
object (self)
  inherit source

  (** [get_next_file] is supposed to return "quickly".
    * This means that no resolving should be done here. *)
  method virtual get_next_file : Request.t option

  val mutable remaining = 0

  (** These values are protected by [plock]. *)
  val mutable send_metadata = false
  val mutable current = None
  val plock = Mutex.create ()

  (** How to unload a request. *)
  method private end_track =
    Mutex.lock plock ;
    begin match current with
        | None -> ()
        | Some (request,_,close) ->
            begin match Request.get_filename request with
              | None ->
                  self#log 1
                    ( "Finished with a non-existent file ?! " ^
                      "Something may have been moved or destroyed during " ^
                      "decoding. It is VERY dangerous, avoid it !" )
              | Some f -> self#logl 3 (lazy (Log.f "Finished with %S" f))
            end ;
            close () ;
            Request.destroy request
    end ;
    current <- None ;
    remaining <- 0 ;
    Mutex.unlock plock

  (** Load a request.
    * Should be called within critical section,
    * when there is no ready request. *)
  method private begin_track =
    assert (not (Mutex.try_lock plock)) ;
    assert (current = None) ;
    match self#get_next_file with
      | None ->
          self#log 5 "Failed to prepare track: no file" ;
          false
      | Some req when Request.is_ready req ->
          (* [Request.is_ready] ensures that we can get a filename from
           * the request, and it can be decoded. *)
          let file = match Request.get_filename req with
            | None -> assert false
            | Some f -> f
          in
          let decoder = match Request.get_decoder req with
            | None -> assert false
            | Some d -> d
          in
            self#logl 3 (lazy (Log.f "Prepared %S -- rid %d"
                                 file (Request.get_id req))) ;
            current <-
              Some (req,
                    (fun buf -> (remaining <- decoder.Decoder.fill buf)),
                    decoder.Decoder.close) ;
            remaining <- (-1) ;
            send_metadata <- true ;
            true
      | Some req ->
          (* We got an unresolved request.. this shoudn't actually happen *)
          self#log 1 "Failed to prepare track: unresolved request" ;
          Request.destroy req ;
          false

  (** Now we can write the source's methods. *)

  val mutable must_fail = false

  method is_ready =
    Mutex.lock plock ;
    let ans = current <> None || must_fail || self#begin_track in
      Mutex.unlock plock ;
      ans

  method remaining = remaining

  method private get_frame buf =
    if must_fail then begin
      must_fail <- false ;
      Mixer.Buffer.add_break buf (Mixer.Buffer.position buf)
    end else begin
      let rec try_get () =
        match current with
          | None ->
              if self#begin_track then try_get ()
          | Some (req,get_frame,_) ->
              if send_metadata then begin
                Request.on_air req ;
                Mixer.Buffer.set_metadata buf
                  (Mixer.Buffer.position buf)
                  (Request.get_all_metadata req) ;
                send_metadata <- false
              end ;
              get_frame buf
      in
        Mutex.lock plock ;
        try_get () ;
        Mutex.unlock plock ;
        if Mixer.Buffer.is_partial buf then self#end_track
    end

  method abort_track =
    self#end_track ;
    must_fail <- true

  method private sleep = self#end_track

  method copy_queue =
    match current with
    | None -> []
    | Some (r,_,_) -> [r]

end

(** Same thing, with a queue in which we prefetch files,
  * which requests are given by [get_next_request].
  * Heuristical settings determining how the source feeds its queue:
  * - the source tries to have more than [length] seconds in queue
  * - if the duration of a file is unknown we use [default_duration] seconds
  * - downloading a file is required to take less than [timeout] seconds *)
class virtual queued ?(length=60.) ?(default_duration=30.) ?(timeout=20.) () =
object (self)
  inherit unqueued as super

  method virtual get_next_request : Request.t option

  (** Management of the queue of files waiting to be played. *)
  val min_queue_length = int_of_float (length/.Mixer.Buffer.length)
  val qlock = Mutex.create ()
  val retrieved = Queue.create ()
  val mutable queue_length = 0 (* Frames *)
  val mutable resolving = None

  (** Launch the feeding task when the source wakes up,
    * finish it when the source goes to sleep. *)
  val mutable sleeping = true
  val mutable task_id = None

  method private wake_up activation =
    sleeping <- false ;
    Tutils.Task.create ~k:(fun i -> task_id <- Some i) self#feed_queue

  method private sleep =
    sleeping <- true ;
    task_id <- None ;
    super#sleep

  (** This method should be called whenever the feeding task has a new
    * opportunity to feed the queue, in case it is sleeping. *)
  method private notify_new_request =
    match task_id with
      | None -> assert false
      | Some id -> Tutils.Task.wake_up id

  (** The body of the feeding task *)
  method private feed_queue () =
    if sleeping then Tutils.Task.Finish else
      (* We keep running as long as we want more audio in the queue
       * and #prefetch believes it can feed the queue *)
      if queue_length < min_queue_length && self#prefetch then
        Tutils.Task.Yield self#feed_queue
      else
        Tutils.Task.Sleep self#feed_queue

  (** Try to feed the queue with a new request.
    * Return false if there was no new request to try,
    * true otherwise, whether the request was fetched successfully or not. *)
  method private prefetch =
    match self#get_next_request with
      | None -> false
      | Some req ->
          resolving <- Some req ;
          begin match Request.resolve req timeout with
            | Request.Resolved ->
                let len =
                  match Request.get_metadata req "duration" with
                    | Some f ->
                        (try float_of_string f with _ -> default_duration)
                    | None -> 100.
                in
                let len = int_of_float (len /. Mixer.Buffer.length) in
                  Mutex.lock qlock ;
                  Queue.add (len,req) retrieved ;
                  self#logl 4 (lazy (Log.f
                                       "queue length %d+=%d (rid %d)"
                                       queue_length len
                                       (Request.get_id req))) ;
                  queue_length <- queue_length + len ;
                  Mutex.unlock qlock ;
                  resolving <- None
              | Request.Failed (* Failure of resolving or decoding *)
              | Request.Timeout ->
                  resolving <- None ;
                  Request.destroy req ;
          end ;
          true

  (** Provide the unqueued [super] with resolved requests. *)
  method private get_next_file =
    Mutex.lock qlock ;
    let ans =
      try
        let len,f = Queue.take retrieved in
          self#logl 4 (lazy (Log.f "queue length %d-=%d"
                               queue_length len)) ;
          queue_length <- queue_length - len ;
          self#notify_new_request ;
          Some f
      with
        | Queue.Empty ->
            self#log 5 "Queue is empty !" ;
            None
    in
      Mutex.unlock qlock ;
      ans

  method copy_queue =
    Mutex.lock qlock ;
    let q =
      match current with
      | None -> []
      | Some (r,_,_) -> [r]
    in
    let q =
      match resolving with
      | None -> q
      | Some r -> r::q
    in
    let q = Queue.fold (fun l r -> (snd r)::l) q retrieved in
      Mutex.unlock qlock ;
      q

end

let queued_proto =
  [ "length", Lang.float_t, Some (Lang.float 60.),
    Some "How much audio (in sec.) should be downloaded in advance." ;
    "default_duration", Lang.float_t, Some (Lang.float 30.),
    Some "When unknown, assume this duration (in sec.) for files." ;
    "timeout", Lang.float_t, Some (Lang.float 20.),
    Some "Timeout (in sec.) for a single download." ]

let extract_queued_params p =
  let l = Lang.to_float (List.assoc "length" p) in
  let d = Lang.to_float (List.assoc "default_duration" p) in
  let t = Lang.to_float (List.assoc "timeout" p) in
    l,d,t
