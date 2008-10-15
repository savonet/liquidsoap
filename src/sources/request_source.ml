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

open Source
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
  method virtual get_next_file : Request.audio Request.t option

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
                  self#log#f 1
                    "Finished with a non-existent file ?! \
                     Something may have been moved or destroyed \
                     during decoding. It is VERY dangerous, avoid it !"
              | Some f -> self#log#f 3 "Finished with %S" f
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
          self#log#f 5 "Failed to prepare track: no file" ;
          false
      | Some req when Request.is_ready req ->
          (* [Request.is_ready] ensures that we can get a filename from
           * the request, and it can be decoded. *)
          let file = Utils.get_some (Request.get_filename req) in
          let decoder = Utils.get_some (Request.get_decoder req) in
            self#log#f 3 "Prepared %S -- rid %d" file (Request.get_id req) ;
            current <-
              Some (req,
                    (fun buf -> (remaining <- decoder.Decoder.fill buf)),
                    decoder.Decoder.close) ;
            remaining <- (-1) ;
            send_metadata <- true ;
            true
      | Some req ->
          (* We got an unresolved request.. this shoudn't actually happen *)
          self#log#f 1 "Failed to prepare track: unresolved request" ;
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
      Frame.add_break buf (Frame.position buf)
    end else begin
      let rec try_get () =
        match current with
          | None ->
              if self#begin_track then try_get ()
          | Some (req,get_frame,_) ->
              if send_metadata then begin
                Request.on_air req ;
                let m = Request.get_all_metadata req in
                Frame.set_metadata buf
                  (Frame.position buf) m;
                send_metadata <- false
              end ;
              get_frame buf
      in
        Mutex.lock plock ;
        try_get () ;
        Mutex.unlock plock ;
        if Frame.is_partial buf then self#end_track
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

(* Private types for request resolutions *)
type resolution = Empty | Retry | Finished

(* Scheduler priority for request resolutions. *)
let priority = Tutils.Maybe_blocking

(** Same thing, with a queue in which we prefetch files,
  * which requests are given by [get_next_request].
  * Heuristical settings determining how the source feeds its queue:
  * - the source tries to have more than [length] seconds in queue
  * - if the duration of a file is unknown we use [default_duration] seconds
  * - downloading a file is required to take less than [timeout] seconds *)
class virtual queued ?(length=60.) ?(default_duration=30.) ?(timeout=20.) () =
object (self)
  inherit unqueued as super

  method virtual get_next_request : Request.audio Request.t option

  (** Management of the queue of files waiting to be played. *)
  val min_queue_length = Fmt.ticks_of_seconds length
  val qlock = Mutex.create ()
  val retrieved = Queue.create ()
  val mutable queue_length = 0 (* Frames *)
  val mutable resolving = None

  (** Asynchronous task for waking up the feeding process.. *)
  val run_m = Mutex.create ()
  val mutable do_run = false
  val mutable wake_task = None 

  (** State should be `Sleeping on awakening, and is then turned to `Running.
    * Eventually #sleep puts it to `Tired, then waits for it to be `Sleeping,
    * meaning that the feeding tasks exited. *)
  val mutable state = `Sleeping
  val state_lock = Mutex.create ()
  val state_cond = Condition.create ()

  method private wake_up activation =
    Tutils.mutexify state_lock
      (fun () ->
         assert (state = `Sleeping) ;
         state <- `Running) () ;
    Mutex.lock run_m; do_run <- true; Mutex.unlock run_m ;
    wake_task <- 
      Some 
        (Duppy.Async.add Tutils.scheduler ~priority
          (fun () ->  Mutex.lock run_m ;
                      if do_run then begin
                        Duppy.Task.add Tutils.scheduler
                         { Duppy.Task.
                           priority = priority ;
                           events   = [`Delay 0.] ;
                           handler  = (fun _ -> self#feed_queue () ; []) } ;
                        do_run <- false
                      end ;
		      Mutex.unlock run_m))

  method private sleep =
    Tutils.mutexify state_lock
      (fun () ->
         assert (state = `Running) ;
         state <- `Tired) () ;
    Tutils.wait state_cond state_lock (fun () -> state = `Sleeping) ;
    super#sleep ;
    begin try
      Mutex.lock qlock ;
      while true do
        let (_,req) = Queue.take retrieved in
          Request.destroy req
      done
    with e -> Mutex.unlock qlock ; if e <> Queue.Empty then raise e end ;
    let task = Utils.get_some wake_task in
      wake_task <- None ;
      Duppy.Async.stop task

  (** This method should be called whenever the feeding task has a new
    * opportunity to feed the queue, in case it is sleeping. *)
  method private notify_new_request =
    match wake_task with
      | Some task -> Duppy.Async.wake_up task
      | None -> ()

  (** A function that returns delays for tasks, making sure that these tasks
    * don't repeat too fast.
    * The current scheme is to return 0. as long as there are no more than
    * [max] consecutive occurences separated by less than [delay], otherwise
    * return [delay]. *)
  val adaptative_delay =
    let last   = ref 0. in
    let excess = ref 0  in
    let delay = 2. in
    let max   = 3  in
    let next () =
      let now = Unix.gettimeofday () in
        if now -. !last < delay then incr excess else excess := 0 ;
        last := now ;
        if !excess >= max then delay else 0.
    in
      next

  (** The body of the feeding task *)
  method private feed_queue () : unit =
    let put_on_sleep () =
      Mutex.lock run_m ;
      do_run <- true ;
      Mutex.unlock run_m
    in
    (* If the test fails, the task ends.. *)
    if
      Tutils.mutexify state_lock
        (fun () ->
           if state <> `Tired then true else begin
             state <- `Sleeping ;
             Condition.signal state_cond ;
             false
           end) ()
    then
      if queue_length < min_queue_length then
        match self#prefetch with
          | Finished ->
              Duppy.Task.add Tutils.scheduler
                { Duppy.Task.
                    priority = priority ;
                    events   = [`Delay 0.] ;
                    handler  =  (fun _ -> self#feed_queue (); []) }
          | Retry ->
              (* Reschedule the task later *)
              Duppy.Task.add Tutils.scheduler
                { Duppy.Task.
                    priority  = priority ;
                    events   = [`Delay (adaptative_delay ())] ;
                    handler  = (fun _ -> self#feed_queue (); []) }
          | Empty -> put_on_sleep ()
      else put_on_sleep ()

  (** Try to feed the queue with a new request.
    * Return false if there was no new request to try,
    * true otherwise, whether the request was fetched successfully or not. *)
  method private prefetch =
    match self#get_next_request with
      | None -> Empty
      | Some req ->
          resolving <- Some req ;
          begin match Request.resolve req timeout with
            | Request.Resolved ->
                let len =
                  match Request.get_metadata req "duration" with
                    | Some f ->
                        (try float_of_string f with _ -> default_duration)
                    | None -> default_duration
                in
                let len =
                  int_of_float (len *. float (Fmt.ticks_per_second()))
                in
                  Mutex.lock qlock ;
                  Queue.add (len,req) retrieved ;
                  self#log#f 4 "queue length %d+=%d (rid %d)"
                    queue_length len (Request.get_id req) ;
                  queue_length <- queue_length + len ;
                  Mutex.unlock qlock ;
                  resolving <- None ;
                  Finished
              | Request.Failed (* Failure of resolving or decoding *)
              | Request.Timeout ->
                  resolving <- None ;
                  Request.destroy req ;
                  Retry
          end

  (** Provide the unqueued [super] with resolved requests. *)
  method private get_next_file =
    Mutex.lock qlock ;
    let ans =
      try
        let len,f = Queue.take retrieved in
          self#log#f 4 "queue length %d-=%d" queue_length len ;
          queue_length <- queue_length - len ;
          Some f
      with
        | Queue.Empty ->
            self#log#f 5 "Queue is empty !" ;
            None
    in
      Mutex.unlock qlock ;
      self#notify_new_request ;
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
