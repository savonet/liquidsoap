(*****************************************************************************

   Liquidsoap, a programmable stream generator.
   Copyright 2003-2024 Savonet team

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
module Queue = Liquidsoap_lang.Queues.Queue

let conf_prefetch =
  Dtools.Conf.int ~p:(Request.conf#plug "prefetch") ~d:1 "Default prefetch"

(* Scheduler priority for request resolutions. *)
let priority = `Maybe_blocking

type queue_item = {
  request : Request.t;
  (* in seconds *)
  mutable expired : bool;
}

type handler = {
  req : Request.t;
  fread : int -> Frame.t;
  seek : int -> int;
  close : unit -> unit;
}

type task = { notify : unit -> unit; stop : unit -> unit }

let log_failed_request (log : Log.t) request ans =
  log#important "Could not resolve request %s: %s."
    (Request.initial_uri request)
    (match ans with
      | `Failed -> "failed"
      | `Timeout -> "timeout"
      | `Resolved -> "file could not be decoded with the correct content")

let should_fail = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"request.dynamic shutdown" (fun () ->
      Atomic.set should_fail true)

class dynamic ?(name = "request.dynamic") ~retry_delay ~available ~prefetch
  ~synchronous ~timeout f =
  let available () = (not (Atomic.get should_fail)) && available () in
  object (self)
    inherit source ~name ()
    method fallible = true
    val mutable remaining = 0
    method remaining = remaining
    val mutable first_fill = false
    val mutable current = Atomic.make None
    method current = Atomic.get current
    method self_sync = (`Static, None)
    val should_skip = Atomic.make false

    (** How to unload a request. *)
    method private end_request =
      Atomic.set should_skip false;
      remaining <- 0;
      match Atomic.exchange current None with
        | None -> ()
        | Some cur -> begin
            (match Request.get_filename cur.req with
              | None -> ()
              | Some f -> self#log#info "Finished with %S." f);
            cur.close ();
            Request.done_playing ~source:(self :> Source.source) cur.req;
            Request.destroy cur.req
          end

    method private fetch_request =
      assert (self#current = None);
      try
        match self#get_next_file with
          | `Empty -> false
          | `Request req
            when Request.resolved req
                 && Request.has_decoder ~ctype:self#content_type req ->
              (* [Request.resolved] ensures that we can get a filename from the request,
                 and it can be decoded. *)
              let file = Option.get (Request.get_filename req) in
              let decoder =
                Option.get (Request.get_decoder ~ctype:self#content_type req)
              in
              self#log#important "Prepared %s (RID %d)."
                (Lang_string.quote_string file)
                (Request.id req);

              (* We use this mutex to avoid seeking and filling at the same time.. *)
              let m = Mutex.create () in
              Atomic.set current
                (Some
                   {
                     req;
                     fread =
                       Mutex_utils.mutexify m (fun len ->
                           let buf = decoder.Decoder.fread len in
                           remaining <- decoder.Decoder.remaining ();
                           buf);
                     seek =
                       Mutex_utils.mutexify m (fun len ->
                           decoder.Decoder.fseek len);
                     close = decoder.Decoder.fclose;
                   });
              remaining <- decoder.Decoder.remaining ();
              Request.on_air ~source:(self :> Source.source) req;
              first_fill <- true;
              true
          | `Request req ->
              (* We got an unresolved request.. this shouldn't actually happen *)
              self#log#critical "Failed to prepare track: request not ready.";
              Request.destroy req;
              false
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Failed to fetch a new request: %s"
             (Printexc.to_string exn));
        false

    method private generate_from_current_request len =
      match self#current with
        | None -> assert false
        | Some cur ->
            let buf = cur.fread len in
            if first_fill then (
              let m = Request.metadata cur.req in
              let buf = Frame.add_metadata buf 0 m in
              let buf = Frame.add_track_mark buf 0 in
              first_fill <- false;
              buf)
            else buf

    method private generate_frame =
      let size = Lazy.force Frame.size in
      let rec fill buf =
        let pos = Frame.position buf in
        if pos < size then (
          let buf =
            Frame.append buf (self#generate_from_current_request (size - pos))
          in
          if Atomic.get should_skip || Frame.is_partial buf then (
            self#end_request;
            let buf = Frame.add_track_mark buf (Frame.position buf) in
            if self#fetch_request then fill buf else buf)
          else buf)
        else buf
      in
      let buf = fill self#empty_frame in

      (* At an end of track, we always have unqueued#remaining=0, so there's
         nothing special to do. *)
      if self#queue_size < prefetch then self#notify_new_request;

      buf

    method! seek x =
      match self#current with None -> 0 | Some cur -> cur.seek x

    method seek_source = (self :> Source.source)
    method abort_track = Atomic.set should_skip true

    method private is_request_ready =
      self#current <> None || try self#fetch_request with _ -> false

    method can_generate_frame =
      match self#is_request_ready with
        | true -> true
        | false ->
            if available () then self#notify_new_request;
            (* Try one more time in case a new request was queued above. *)
            self#is_request_ready

    val retrieved : queue_item Queue.t = Queue.create ()
    method private queue_size = Queue.length retrieved
    method queue = retrieved

    method set_queue =
      self#clear_retrieved;
      List.iter (fun request ->
          match Request.resolve ?timeout request with
            | `Resolved
              when Request.has_decoder ~ctype:self#content_type request ->
                Queue.push retrieved { request; expired = false }
            | ans -> log_failed_request self#log request ans)

    method add i =
      match Request.resolve ?timeout i.request with
        | `Resolved when Request.has_decoder ~ctype:self#content_type i.request
          ->
            Queue.push retrieved i;
            true
        | ans ->
            log_failed_request self#log i.request ans;
            false

    val state = Atomic.make `Sleeping

    initializer
      self#on_wake_up (fun () ->
          let task =
            if synchronous then
              {
                notify = (fun () -> self#synchronous_feed_queue);
                stop = (fun () -> ());
              }
            else (
              let t =
                Duppy.Async.add Tutils.scheduler ~priority self#feed_queue
              in
              {
                notify = (fun () -> Duppy.Async.wake_up t);
                stop = (fun () -> Duppy.Async.stop t);
              })
          in
          assert (
            Atomic.compare_and_set state `Sleeping
              (`Started (Unix.gettimeofday (), task))))

    method private clear_retrieved =
      let rec clear () =
        match Queue.pop_opt retrieved with
          | None -> ()
          | Some { request = req; _ } ->
              Request.destroy req;
              clear ()
      in
      clear ()

    initializer
      self#on_sleep (fun () ->
          match Atomic.exchange state `Sleeping with
            | `Started (_, { stop }) ->
                stop ();
                (* No more feeding task, we can go to sleep. *)
                self#end_request;
                self#log#info "Cleaning up request queue...";
                self#clear_retrieved
            | _ -> assert false)

    (** This method should be called whenever the feeding task has a new
        opportunity to feed the queue, in case it is sleeping. *)
    method private notify_new_request =
      match Atomic.get state with
        | `Started (d, { notify }) when d <= Unix.gettimeofday () -> notify ()
        | _ -> ()

    (** The body of the feeding task *)
    method private feed_queue () =
      match (self#queue_size < prefetch, Atomic.get state) with
        | true, `Started (d, t) when d <= Unix.gettimeofday () -> (
            match self#fetch with
              | `Finished -> if self#queue_size < prefetch then 0. else -1.
              | `Retry ->
                  let d = retry_delay () in
                  Atomic.set state (`Started (Unix.gettimeofday () +. d, t));
                  d)
        | _ -> -1.

    method private synchronous_feed_queue =
      match self#feed_queue () with
        | 0. -> self#synchronous_feed_queue
        | _ -> ()

    method fetch =
      try
        let r =
          if available () then (
            match
              Lang.to_valued_option Request.Value.of_value (Lang.apply f [])
            with
              | Some r -> `Request r
              | None -> `Retry
              | exception exn ->
                  let bt = Printexc.get_backtrace () in
                  Utils.log_exception ~log:self#log ~bt
                    (Printf.sprintf "Failed to obtain a media request: %s"
                       (Printexc.to_string exn));
                  `Retry)
          else `Retry
        in
        match r with
          | `Retry -> `Retry
          | `Request req -> (
              match Request.resolve ?timeout req with
                | `Resolved
                  when Request.has_decoder ~ctype:self#content_type req ->
                    let rec remove_expired ret =
                      match Queue.pop_opt retrieved with
                        | None -> List.rev ret
                        | Some r ->
                            let ret =
                              if r.expired then (
                                self#log#info "Dropping expired request.";
                                Request.destroy r.request;
                                ret)
                              else r :: ret
                            in
                            remove_expired ret
                    in
                    List.iter
                      (fun r -> Queue.push retrieved r)
                      (remove_expired []);
                    Queue.push retrieved { request = req; expired = false };
                    self#log#info "Queued %d request(s)" self#queue_size;
                    `Finished
                | _ ->
                    Request.destroy req;
                    `Retry)
      with exn ->
        let bt = Printexc.get_backtrace () in
        Utils.log_exception ~log:self#log ~bt
          (Printf.sprintf "Error while fetching next request: %s"
             (Printexc.to_string exn));
        `Retry

    (** Provide the unqueued [super] with resolved requests. *)
    method private get_next_file =
      match Queue.pop_opt retrieved with
        | None -> `Empty
        | Some r ->
            self#log#info "Remaining %d requests" self#queue_size;
            `Request r.request
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
          "Retry after a given time (in seconds) when callback returns `null` \
           or an error occurs while resolving a returned request." );
      ( "available",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Whether some new requests are available (when set to false, it \
           stops after current playing request)." );
      ( "synchronous",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "If `true`, new requests are prepared as needed instead of using an \
           asynchronous queue." );
      ( "prefetch",
        Lang.nullable_t Lang.int_t,
        Some Lang.null,
        Some "How many requests should be queued in advance." );
      ( "timeout",
        Lang.nullable_t Lang.float_t,
        Some Lang.null,
        Some
          "Timeout (in sec.) to resolve the request. Defaults to \
           `settings.request.timeout` when `null`." );
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
                  | `Retry ->
                      log#important "Fetch failed";
                      Lang.bool false) );
        ( "queue",
          ([], Lang.fun_t [] (Lang.list_t Request.Value.t)),
          "Get the requests currently in the queue.",
          fun s ->
            Lang.val_fun [] (fun _ ->
                let requests =
                  List.map
                    (fun r -> Request.Value.to_value r.request)
                    (Queue.elements s#queue)
                in
                Lang.list requests) );
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
                s#set_queue l;
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
      let prefetch =
        Lang.to_valued_option Lang.to_int (List.assoc "prefetch" p)
      in
      let prefetch = Option.value ~default:conf_prefetch#get prefetch in
      let synchronous = Lang.to_bool (List.assoc "synchronous" p) in
      let timeout =
        Lang.to_valued_option Lang.to_float (List.assoc "timeout" p)
      in
      new dynamic ~available ~retry_delay ~prefetch ~timeout ~synchronous f)
