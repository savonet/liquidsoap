(*****************************************************************************

   Liquidsoap, a programmable stream generator.
   Copyright 2003-2019 Savonet team

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

open Source

type handler = {
  req : Request.t;
  fill : Frame.t -> unit;
  seek : int -> int;
  close : unit -> unit;
}

(** Class [unqueued] plays the file given by method [get_next_file] as a request
    which is ready, i.e. has been resolved. On the top of it we define [queued],
    which manages a queue of files, feed by resolving in an other thread requests
    given by [get_next_request]. *)
class virtual unqueued ~kind ~name =
  object (self)
    inherit source kind ~name

    (** [get_next_file] returns a ready audio request. It is supposed to return
      "quickly", which means that no resolving can be done here. *)
    method virtual get_next_file : Request.t option

    val mutable remaining = 0

    val mutable must_fail = false

    (** These values are protected by [plock]. *)
    val mutable send_metadata = false

    val mutable current = None

    val plock = Mutex.create ()

    method self_sync = false

    (** How to unload a request. *)
    method private end_track forced =
      Mutex.lock plock;
      begin
        match current with
        | None -> ()
        | Some cur ->
            begin
              match Request.get_filename cur.req with
              | None ->
                  self#log#severe
                    "Finished with a non-existent file?! Something may have \
                     been moved or destroyed during decoding. It is VERY \
                     dangerous, avoid it!"
              | Some f -> self#log#info "Finished with %S." f
            end;
            cur.close ();
            Request.destroy cur.req;
            must_fail <- forced
      end;
      current <- None;
      remaining <- 0;
      Mutex.unlock plock

    (** Load a request. Should be called within critical section, when there is no
      ready request. *)
    method private begin_track =
      assert (Tutils.seems_locked plock);
      assert (current = None);
      match self#get_next_file with
        | None ->
            self#log#debug "Failed to prepare track: no file.";
            false
        | Some req when Request.is_ready req ->
            assert (Frame.kind_sub_kind (Utils.get_some (Request.kind req)) kind);

            (* [Request.is_ready] ensures that we can get a filename from the request,
         and it can be decoded. *)
            let file = Utils.get_some (Request.get_filename req) in
            let decoder = Utils.get_some (Request.get_decoder req) in
            self#log#important "Prepared %S (RID %d)." file (Request.get_id req);

            (* We use this mutex to avoid seeking and filling at the same time.. *)
            let m = Mutex.create () in
            current <-
              Some
                {
                  req;
                  fill =
                    Tutils.mutexify m (fun buf ->
                        remaining <- decoder.Decoder.fill buf);
                  seek =
                    Tutils.mutexify m (fun len -> decoder.Decoder.fseek len);
                  close = decoder.Decoder.close;
                };
            remaining <- -1;
            send_metadata <- true;
            true
        | Some req ->
            (* We got an unresolved request.. this shoudn't actually happen *)
            self#log#critical "Failed to prepare track: request not ready.";
            Request.destroy req;
            false

    (** Now we can write the source's methods. *)

    method is_ready =
      Mutex.lock plock;
      let ans = current <> None || must_fail || self#begin_track in
      Mutex.unlock plock;
      ans

    method remaining = remaining

    method private get_frame buf =
      if must_fail then (
        must_fail <- false;
        Frame.add_break buf (Frame.position buf) )
      else (
        let try_get () =
          match current with
            | None ->
                (* We're supposed to be ready so this shouldn't be reached. *)
                assert false
            | Some cur ->
                if send_metadata then (
                  Request.on_air cur.req;
                  let m = Request.get_all_metadata cur.req in
                  Frame.set_metadata buf (Frame.position buf) m;
                  send_metadata <- false );
                cur.fill buf
        in
        Tutils.mutexify plock try_get ();
        if Frame.is_partial buf then self#end_track false )

    method seek x = match current with None -> 0 | Some cur -> cur.seek x

    method abort_track = self#end_track true

    method private sleep = self#end_track false

    method copy_queue = match current with None -> [] | Some cur -> [cur.req]
  end

type queue_item = {
  request : Request.t;
  duration : float;
  (* in seconds *)
  mutable expired : bool;
}

(* Private types for request resolutions *)
type resolution = Empty | Retry | Finished

(* Scheduler priority for request resolutions. *)
let priority = Tutils.Maybe_blocking

(** Same thing, with a queue in which we prefetch files, which requests are
    given by [get_next_request]. Heuristical settings determining how the source
    feeds its queue:
    - the source tries to have more than [length] seconds in queue
    - if the duration of a file is unknown we use [default_duration] seconds
    - downloading a file is required to take less than [timeout] seconds
   *)
class virtual queued ~kind ~name ?(length = 10.) ?(default_duration = 30.)
  ?(conservative = false) ?(timeout = 20.) () =
  object (self)
    inherit unqueued ~kind ~name as super

    method stype = Fallible

    method virtual get_next_request : Request.t option

    (** Management of the queue of files waiting to be played. *)
    val min_queue_length = length

    val qlock = Mutex.create ()

    val retrieved : queue_item Queue.t = Queue.create ()

    val mutable queue_length = 0.

    (* Seconds *)
    val mutable resolving = None

    (** [available_length] is the length of the queue plus the remaining time in
      the current track (0 in conservative mode). Although [queue_length]
      should be accessed under [qlock] we don't do it here, and more
      importantly, all the instances of "if self#available_length <
      min_queue_length then ..." are not run within [qlock]'s critical
      section. The potential race condition (there seems to be enough, then the
      length decreases but we have already decided to take no action) is
      harmless because all the portions of the code that decrease the length
      trigger the appropriate action (feeding) themselves. *)
    method private available_length =
      if conservative then queue_length
      else (
        let remaining = self#remaining in
        if remaining < 0 then
          (* There is a track available but we don't know its duration at this
           point. Hence, using default_duration. *)
          queue_length +. default_duration
        else queue_length +. Frame.seconds_of_master remaining )

    (** State should be `Sleeping on awakening, and is then turned to `Running.
      Eventually #sleep puts it to `Tired, then waits for it to be `Sleeping,
      meaning that the feeding task exited. *)
    val mutable state = `Sleeping

    val state_lock = Mutex.create ()

    val state_cond = Condition.create ()

    val mutable task = None

    method private wake_up activation =
      (* Not for unqueued#wake_up but source#wake_up performs some logging. *)
      super#wake_up activation;
      assert (task = None);
      Tutils.mutexify state_lock
        (fun () ->
          assert (state = `Sleeping);
          let t = Duppy.Async.add Tutils.scheduler ~priority self#feed_queue in
          Duppy.Async.wake_up t;
          task <- Some t;
          state <- `Running)
        ()

    method private sleep =
      (* We need to be sure that the feeding task stopped filling the queue
       before we destroy all requests from that queue.  Async.stop only
       promises us that on the next round the task will stop but won't tell us
       if it's currently resolving a file or not.  So we first put the queue
       into an harmless state: we put the state to `Tired and wait for it to
       acknowledge it by setting it to `Sleeping. *)
      Tutils.mutexify state_lock
        (fun () ->
          assert (state = `Running);
          state <- `Tired)
        ();

      (* Make sure the task is awake so that it can see our signal. *)
      Duppy.Async.wake_up (Utils.get_some task);
      self#log#info "Waiting for feeding task to stop...";
      Tutils.wait state_cond state_lock (fun () -> state = `Sleeping);
      Duppy.Async.stop (Utils.get_some task);
      task <- None;

      (* No more feeding task, we can go to sleep. *)
      super#sleep;
      self#log#info "Cleaning up request queue...";
      try
        Mutex.lock qlock;
        while true do
          let { request = req; _ } = Queue.take retrieved in
          Request.destroy req
        done
      with e ->
        Mutex.unlock qlock;
        if e <> Queue.Empty then raise e

    (** This method should be called whenever the feeding task has a new
      opportunity to feed the queue, in case it is sleeping. *)
    method private notify_new_request =
      (* Avoid trying to wake up the task during the shutdown process where it
       might have been stopped already, in which case we'd get an
       exception. *)
      Tutils.mutexify state_lock
        (fun () ->
          if state = `Running then Duppy.Async.wake_up (Utils.get_some task))
        ()

    (** A function that returns delays for tasks, making sure that these tasks
      don't repeat too fast. The current scheme is to return 0. as long as there
      are no more than [max] consecutive occurences separated by less than
      [delay], otherwise return [delay]. *)
    val adaptative_delay =
      let last = ref 0. in
      let excess = ref 0 in
      let delay = 2. in
      let max = 3 in
      let next () =
        let now = Unix.gettimeofday () in
        if now -. !last < delay then incr excess else excess := 0;
        last := now;
        if !excess >= max then delay else 0.
      in
      next

    (** The body of the feeding task *)
    method private feed_queue () =
      if
        (* Is the source running? And does it need prefetching? If the test fails,
         the task sleeps. *)
        Tutils.mutexify state_lock
          (fun () ->
            match state with
              | `Running -> true
              | `Tired ->
                  state <- `Sleeping;
                  Condition.signal state_cond;
                  false
              | `Sleeping ->
                  (* Because many calls to wake_up result in waking up
                   * many times, it is possible that we wake up twice
                   * after #sleep initiates sleeping by setting `Tired.
                   * The second time we see `Sleeping, and we should not
                   * do anything.
                   * At some point the task will be stopped and it will
                   * disappear.
                   * What we need is:
                   *  - That the task disappears once the source sleeps,
                   *    and we pretty much get it (modulo small delay).
                   *  - That the request queue can be emptied for good,
                   *    and be not re-fed at the same time. This is the
                   *    case because state = Sleeping in all rounds of
                   *    the task when the queue can be cleaned. (It is
                   *    possible to start a round before Async.stop and
                   *    execute its content while the queue is being
                   *    emptied, but Sleeping saves us). *)
                  false)
          ()
        && self#available_length < min_queue_length
      then (
        match self#prefetch with
          | Finished -> 0.
          | Retry -> adaptative_delay ()
          | Empty -> -1. )
      else -1.

    (** Try to feed the queue with a new request. Return a resolution status:
      Empty if there was no new request to try,
      Retry if there was a new one but it failed to be resolved,
      Finished if all went OK. *)
    method private prefetch =
      match self#get_next_request with
        | None -> Empty
        | Some req -> (
            resolving <- Some req;
            match Request.resolve req timeout with
              | Request.Resolved ->
                  let len =
                    match Request.get_metadata req "duration" with
                      | Some f -> (
                          try float_of_string f with _ -> default_duration )
                      | None -> default_duration
                  in
                  let rec remove_expired n =
                    if n = 0 then ()
                    else (
                      let r = Queue.take retrieved in
                      if r.expired then (
                        self#log#info "Dropping expired request.";
                        Request.destroy r.request )
                      else Queue.add r retrieved;
                      remove_expired (n - 1) )
                  in
                  Mutex.lock qlock;
                  remove_expired (Queue.length retrieved);
                  Queue.add
                    { request = req; duration = len; expired = false }
                    retrieved;
                  self#log#info
                    "Remaining: %.1fs, queued: %.1fs, adding: %.1fs (RID %d)"
                    (Frame.seconds_of_master self#remaining)
                    queue_length len (Request.get_id req);
                  queue_length <- queue_length +. len;
                  Mutex.unlock qlock;
                  resolving <- None;
                  Finished
              | Request.Failed (* Failure of resolving or decoding *)
              | Request.Timeout ->
                  resolving <- None;
                  Request.destroy req;
                  Retry )

    (** Provide the unqueued [super] with resolved requests. *)
    method private get_next_file =
      Mutex.lock qlock;
      let ans =
        try
          let r = Queue.take retrieved in
          self#log#info "Remaining: %.1fs, queued: %.1fs, taking: %.1fs"
            (Frame.seconds_of_master self#remaining)
            queue_length r.duration;
          if not r.expired then queue_length <- queue_length -. r.duration;
          Some r.request
        with Queue.Empty ->
          self#log#debug "Queue is empty!";
          None
      in
      Mutex.unlock qlock;

      (* A request has been taken off the queue, there is a chance that the
       queue should be refilled: awaken the feeding task. However, we can wait
       that this file is played, and this need will be noticed in #get_frame.
       This is critical in non-conservative mode because at this point
       remaining is still 0 (we're inside #begin_track) which would lead to
       too early prefetching; if we wait that a frame has been produced, we'll
       get the first non-infinite remaining time estimations. *)
      ans

    method private expire test =
      let already_short = self#available_length < min_queue_length in
      Mutex.lock qlock;
      Queue.iter
        (fun r ->
          if test r.request && not r.expired then (
            r.expired <- true;
            queue_length <- queue_length -. r.duration ))
        retrieved;
      Mutex.unlock qlock;
      if self#available_length < min_queue_length then (
        if not already_short then
          self#log#info "Expirations made the queue too short, feeding...";

        (* Notify in any case, notifying twice never hurts. *)
        self#notify_new_request )

    method private get_frame ab =
      super#get_frame ab;

      (* At an end of track, we always have unqueued#remaining=0, so there's
       nothing special to do. *)
      if self#available_length < min_queue_length then self#notify_new_request

    method copy_queue =
      Mutex.lock qlock;
      let q = match current with None -> [] | Some cur -> [cur.req] in
      let q = match resolving with None -> q | Some r -> r :: q in
      let q = Queue.fold (fun l r -> r.request :: l) q retrieved in
      Mutex.unlock qlock;
      q
  end

let queued_proto =
  [
    ( "length",
      Lang.float_t,
      Some (Lang.float 40.),
      Some "How much audio (in sec.) should be queued in advance." );
    ( "default_duration",
      Lang.float_t,
      Some (Lang.float 30.),
      Some "When unknown, assume this duration (in sec.) for files." );
    ( "conservative",
      Lang.bool_t,
      Some (Lang.bool false),
      Some
        "If true, estimated remaining time on the current track is not \
         considered when computing queue length." );
    ( "timeout",
      Lang.float_t,
      Some (Lang.float 20.),
      Some "Timeout (in sec.) for a single download." );
  ]

let extract_queued_params p =
  let l = Lang.to_float (List.assoc "length" p) in
  let d = Lang.to_float (List.assoc "default_duration" p) in
  let t = Lang.to_float (List.assoc "timeout" p) in
  let c = Lang.to_bool (List.assoc "conservative" p) in
  (l, d, t, c)
