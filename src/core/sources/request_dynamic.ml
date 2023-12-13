(*****************************************************************************

   Liquidsoap, a programmable stream generator.
   Copyright 2003-2023 Savonet team

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

(* Scheduler priority for request resolutions. *)
let priority = `Maybe_blocking

type queue_item = {
  request : Request.t;
  (* in seconds *)
  mutable expired : bool;
}

type handler = {
  req : Request.t;
  fill : Frame.t -> unit;
  seek : int -> int;
  close : unit -> unit;
}

let log_failed_request (log : Log.t) request ans =
  log#important "Could not resolve request %s: %s."
    (Request.initial_uri request)
    (match ans with
      | Request.Failed -> "failed"
      | Request.Timeout -> "timeout"
      | Request.Resolved -> assert false)

let extract_queued_params p =
  let l = Lang.to_int (List.assoc "prefetch" p) in
  let t = Lang.to_float (List.assoc "timeout" p) in
  (l, t)

class dynamic ~retry_delay ~available (f : Lang.value) prefetch timeout =
  let should_fail = Atomic.make false in
  let available () = (not (Atomic.get should_fail)) && available () in
  object (self)
    initializer
      Lifecycle.before_core_shutdown
        ~name:(Printf.sprintf "%s shutdown" self#id) (fun () ->
          Atomic.set should_fail true)

    inherit source ~name:"request.dynamic" () as super
    method stype = `Fallible
    val mutable remaining = 0
    method remaining = remaining
    val mutable must_fail = false

    (** These values are protected by [plock]. *)
    val mutable send_metadata = false

    val mutable current = None
    method current = current
    val plock = Mutex.create ()
    method self_sync = (`Static, false)

    (** How to unload a request. *)
    method private end_track =
      Tutils.mutexify plock (fun forced ->
          begin
            match current with
              | None -> ()
              | Some cur ->
                  begin
                    match Request.get_filename cur.req with
                      | None ->
                          self#log#severe
                            "Finished with a non-existent file?! Something may \
                             have been moved or destroyed during decoding. It \
                             is VERY dangerous, avoid it!"
                      | Some f -> self#log#info "Finished with %S." f
                  end;
                  cur.close ();
                  Request.destroy cur.req;
                  must_fail <- forced
          end;
          current <- None;
          remaining <- 0)

    (** Load a request. Should be called within critical section, when there is no
      ready request. *)
    method private begin_track =
      assert (Tutils.seems_locked plock);
      assert (current = None);
      match self#get_next_file with
        | `Retry _ | `Empty ->
            self#log#debug "Failed to prepare track: no file.";
            false
        | `Request req when Request.resolved req && Request.ctype req <> None ->
            assert (
              Frame.compatible
                (Option.get (Request.ctype req))
                self#content_type);

            (* [Request.resolved] ensures that we can get a filename from the request,
               and it can be decoded. *)
            let file = Option.get (Request.get_filename req) in
            let decoder = Option.get (Request.get_decoder req) in
            self#log#important "Prepared %s (RID %d)."
              (Lang_string.quote_string file)
              (Request.get_id req);

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
        | `Request req ->
            (* We got an unresolved request.. this shouldn't actually happen *)
            self#log#critical "Failed to prepare track: request not ready.";
            Request.destroy req;
            false

    method private get_frame buf =
      let end_track =
        Tutils.mutexify plock
          (fun () ->
            if must_fail then (
              must_fail <- false;
              Frame.add_break buf (Frame.position buf);
              false)
            else (
              match current with
                | None ->
                    (* We're supposed to be ready so this shouldn't be reached. *)
                    assert false
                | Some cur ->
                    if send_metadata then (
                      Request.on_air cur.req;
                      let m = Request.get_all_metadata cur.req in
                      Frame.set_metadata buf (Frame.position buf) m;
                      send_metadata <- false);
                    cur.fill buf;
                    Frame.is_partial buf))
          ()
      in
      if end_track then self#end_track false;

      (* At an end of track, we always have unqueued#remaining=0, so there's
         nothing special to do. *)
      if self#queue_size < prefetch then self#notify_new_request

    method! seek x = match current with None -> 0 | Some cur -> cur.seek x
    method seek_source = (self :> Source.source)
    method abort_track = self#end_track true
    val mutable retry_status = None

    method _is_ready ?frame:_ () =
      let is_ready =
        Tutils.mutexify plock
          (fun () ->
            current <> None || must_fail
            || try self#begin_track with _ -> false)
          ()
      in
      match (is_ready, retry_status) with
        | true, _ -> true
        | false, Some d when Unix.gettimeofday () < d -> false
        | false, _ ->
            if available () then self#notify_new_request;
            false

    method private get_next_request =
      let retry () =
        let delay = retry_delay () in
        retry_status <- Some (Unix.gettimeofday () +. delay);
        `Empty
      in
      if available () then (
        match
          Lang.to_valued_option Request.Value.of_value (Lang.apply f [])
        with
          | Some r ->
              Request.set_root_metadata r "source" (`String self#id);
              `Request r
          | None -> retry ()
          | exception exn ->
              let bt = Printexc.get_backtrace () in
              Utils.log_exception ~log:self#log ~bt
                (Printf.sprintf "Failed to obtain a media request: %s"
                   (Printexc.to_string exn));
              retry ())
      else `Empty

    val qlock = Mutex.create ()
    val retrieved : queue_item Queue.t = Queue.create ()

    method private queue_size =
      self#mutexify (fun () -> Queue.length retrieved) ()

    method queue = self#mutexify (fun () -> Queue.copy retrieved) ()

    method set_queue =
      self#mutexify (fun q ->
          Queue.clear retrieved;
          Queue.iter
            (fun i ->
              match
                Request.resolve ~ctype:(Some self#content_type) i.request
                  timeout
              with
                | Request.Resolved -> Queue.push i retrieved
                | ans -> log_failed_request self#log i.request ans)
            q)

    method add =
      self#mutexify (fun i ->
          match
            Request.resolve ~ctype:(Some self#content_type) i.request timeout
          with
            | Request.Resolved ->
                Queue.push i retrieved;
                true
            | ans ->
                log_failed_request self#log i.request ans;
                false)

    (* Seconds *)
    val mutable resolving = None

    (** State should be `Sleeping on awakening, and is then turned to `Running.
      Eventually #sleep puts it to `Tired, then waits for it to be `Sleeping,
      meaning that the feeding task exited. *)
    val mutable state = `Sleeping

    val state_lock = Mutex.create ()
    val state_cond = Condition.create ()
    val mutable task = None

    method! private wake_up activation =
      (* Not for unqueued#wake_up but source#wake_up performs some logging. *)
      super#wake_up activation;
      assert (task = None);
      Tutils.mutexify state_lock
        (fun () ->
          assert (state = `Sleeping);
          let t = Duppy.Async.add Tutils.scheduler ~priority self#feed_queue in
          Duppy.Async.wake_up t;
          task <- Some t;
          state <- `Starting)
        ()

    method! private sleep =
      if state = `Running then (
        (* We need to be sure that the feeding task stopped filling the queue
           before we destroy all requests from that queue.  Async.stop only
           promises us that on the next round the task will stop but won't tell us
           if it's currently resolving a file or not.  So we first put the queue
           into an harmless state: we put the state to `Tired and wait for it to
           acknowledge it by setting it to `Sleeping. *)
        Tutils.mutexify state_lock (fun () -> state <- `Tired) ();

        (* Make sure the task is awake so that it can see our signal. *)
        Duppy.Async.wake_up (Option.get task);
        self#log#info "Waiting for feeding task to stop...";
        Tutils.wait state_cond state_lock (fun () -> state = `Sleeping));
      Duppy.Async.stop (Option.get task);
      task <- None;

      (* No more feeding task, we can go to sleep. *)
      super#sleep;
      self#end_track false;
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
          if state = `Running then Duppy.Async.wake_up (Option.get task))
        ()

    (** A function that returns delays for tasks, making sure that these tasks
      don't repeat too fast. The current scheme is to return 0. as long as there
      are no more than [max] consecutive occurrences separated by less than
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
              | `Starting ->
                  state <- `Running;
                  true
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
        && self#queue_size < prefetch
      then (
        match self#fetch with
          | `Finished ->
              (* Retry again in order to make sure that we have enough data *)
              0.5
          | `Retry d -> d ()
          | `Empty -> -1.)
      else -1.

    (** Try to feed the queue with a new request. Return a resolution status:
      Empty if there was no new request to try,
      Retry if there was a new one but it failed to be resolved,
      Finished if all went OK. *)
    method fetch =
      match self#get_next_request with
        | `Empty -> `Empty
        | `Retry fn -> `Retry fn
        | `Request req -> (
            resolving <- Some req;
            match
              Request.resolve ~ctype:(Some self#content_type) req timeout
            with
              | Request.Resolved ->
                  let rec remove_expired n =
                    if n = 0 then ()
                    else (
                      let r = Queue.take retrieved in
                      if r.expired then (
                        self#log#info "Dropping expired request.";
                        Request.destroy r.request)
                      else Queue.add r retrieved;
                      remove_expired (n - 1))
                  in
                  Mutex.lock qlock;
                  remove_expired (Queue.length retrieved);
                  Queue.add { request = req; expired = false } retrieved;
                  self#log#info "Queued %d requests" self#queue_size;
                  Mutex.unlock qlock;
                  resolving <- None;
                  `Finished
              | Request.Failed (* Failure of resolving or decoding *)
              | Request.Timeout ->
                  resolving <- None;
                  Request.destroy req;
                  `Retry adaptative_delay)

    (** Provide the unqueued [super] with resolved requests. *)
    method private get_next_file =
      Mutex.lock qlock;
      let ans =
        try
          let r = Queue.take retrieved in
          self#log#info "Remaining %d requests" self#queue_size;
          `Request r.request
        with Queue.Empty ->
          self#log#debug "Queue is empty!";
          `Empty
      in
      Mutex.unlock qlock;

      (* A request has been taken off the queue, there is a chance that the
         queue should be refilled: awaken the feeding task. However, we can wait
         that this file is played, and this need will be noticed in #get_frame. *)
      ans

    method private expire test =
      let already_short = self#queue_size < prefetch in
      Mutex.lock qlock;
      Queue.iter
        (fun r -> if test r.request && not r.expired then r.expired <- true)
        retrieved;
      Mutex.unlock qlock;
      if self#queue_size < prefetch then (
        if not already_short then
          self#log#info "Expirations made the queue too short, feeding...";

        (* Notify in any case, notifying twice never hurts. *)
        self#notify_new_request)
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let log = Log.make ["request"; "dynamic"] in
  Lang.add_operator ~base:Modules.request "dynamic" ~category:`Input
    ~descr:"Play request dynamically created by a given function."
    [
      ("", Lang.fun_t [] (Lang.nullable_t Request.Value.t), None, None);
      ( "retry_delay",
        Lang.getter_t Lang.float_t,
        Some (Lang.float 0.1),
        Some
          "Retry after a given time (in seconds) when callback returns `null`."
      );
      ( "available",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Whether some new requests are available (when set to false, it \
           stops after current playing request)." );
      ( "prefetch",
        Lang.int_t,
        Some (Lang.int 1),
        Some "How many requests should be queued in advance." );
      ( "timeout",
        Lang.float_t,
        Some (Lang.float 20.),
        Some "Timeout (in sec.) for a single download." );
    ]
    ~meth:
      [
        ( "fetch",
          ([], Lang.fun_t [] Lang.bool_t),
          "Try feeding the queue with a new request. Returns `true` if \
           successful. This method can take long to return and should usually \
           be run in a separate thread.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#fetch with
                  | `Finished -> Lang.bool true
                  | `Retry _ ->
                      log#important "Fetch failed: retry.";
                      Lang.bool false
                  | `Empty ->
                      log#important "Fetch failed: empty.";
                      Lang.bool false) );
        ( "queue",
          ([], Lang.fun_t [] (Lang.list_t Request.Value.t)),
          "Get the requests currently in the queue.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                Lang.list
                  (List.rev
                     (Queue.fold
                        (fun c i -> Request.Value.to_value i.request :: c)
                        [] s#queue))) );
        ( "add",
          ([], Lang.fun_t [(false, "", Request.Value.t)] Lang.bool_t),
          "Add a request to the queue. Requests are resolved before being \
           added. Returns `true` if the request was successfully added.",
          fun s ->
            Lang.val_fun
              [("", "", None)]
              (fun p ->
                Lang.bool
                  (s#add
                     {
                       request = Request.Value.of_value (List.assoc "" p);
                       expired = false;
                     })) );
        ( "set_queue",
          ([], Lang.fun_t [(false, "", Lang.list_t Request.Value.t)] Lang.unit_t),
          "Set the queue of requests. Requests are resolved before being added \
           to the queue. You are responsible for destroying the requests \
           currently in the queue.",
          fun s ->
            Lang.val_fun
              [("", "", None)]
              (fun p ->
                let l =
                  List.map Request.Value.of_value
                    (Lang.to_list (List.assoc "" p))
                in
                let q = Queue.create () in
                List.iter
                  (fun request -> Queue.push { request; expired = false } q)
                  l;
                s#set_queue q;
                Lang.unit) );
        ( "current",
          ([], Lang.fun_t [] (Lang.nullable_t Request.Value.t)),
          "Get the request currently being played.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                match s#current with
                  | None -> Lang.null
                  | Some c -> Request.Value.to_value c.req) );
      ]
    ~return_t
    (fun p ->
      let f = List.assoc "" p in
      let available = Lang.to_bool_getter (List.assoc "available" p) in
      let retry_delay = Lang.to_float_getter (List.assoc "retry_delay" p) in
      let l, t = extract_queued_params p in
      new dynamic ~available ~retry_delay f l t)
