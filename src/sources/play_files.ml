
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

  val log = Log.logl ~label:"file"

  val mutable remaining = 0

  (** These values are protected by [plock]. *)

  val mutable current_metadata = None
  val mutable current_file = None
  val mutable current_close = None
  val mutable my_get = None
  val plock = Tutils.Mutex.create "Play_files.plock"

  (** How to unload a request. *)
  method private end_track =
    Mutex.lock plock ;

    ( match current_file,current_close with
        | None, None -> ()
        | (Some file), (Some close) -> (
            match Request.get_filename file with
              | None -> assert false
              | Some f ->
                  log 3 (lazy (Log.f "%s ends %S" id f)) ;
                  close () ;
                  Request.destroy file )
        | _, _ -> assert false ) ;

    current_file <- None ;
    current_close <- None ;
    my_get <- None ;
    remaining <- 0 ;

    Mutex.unlock plock

  (** Load a request. *)
  method private begin_track =
    match self#get_next_file with
      | None -> log 2 (lazy (Log.f "%s failed to begin track -- empty" id))
      | Some req when Request.is_ready req ->

          (* [Request.is_ready] ensures that we can get a filename from
           * the request, and it can be decoded. *)

          let file = match Request.get_filename req with
            | None -> assert false
            | Some f -> f
          in
            log 3 (lazy
                     (Log.f "%s deals with %S -- rid %d"
                        id file (Request.get_id req))) ;

            begin
              match Decoder.get file with
                | Some decoder ->
                    Mutex.lock plock ;

                    current_file <- Some req ;
                    current_close <- Some (decoder.Decoder.close) ;
                    my_get <- Some (
                      fun buf ->
                        (remaining <- decoder.Decoder.fill buf) ) ;
                    remaining <- (-1) ;
                    current_metadata <- Some (Request.get_metadatas req) ;

                    Mutex.unlock plock
                | None -> assert false
            end

      | Some req ->
          log 1 (lazy (Log.f "%s failed to begin track -- not ready" id)) ;
          Request.destroy req

  (** Now we can write the source's methods. *)

  method remaining = remaining
  method is_ready = my_get <> None

  val mutable must_fail = false

  method get buf =
    if must_fail then must_fail <- false else
      begin
        Mutex.lock plock ;
        assert (Mixer.Buffer.is_partial buf) ;
        match my_get with
          | None ->
              Mutex.unlock plock ;
              self#begin_track ;
              if self#is_ready
              then self#get buf
          | Some get ->
              ( match current_metadata,current_file with
                  | None, _ -> ()
                  | Some metadata, Some req ->
                      Request.on_air req ;
                      Mixer.Buffer.push_metadata buf metadata ;
                      current_metadata <- None
                  | Some m, None -> assert false ) ;
              get buf ;
              Mutex.unlock plock ;
              if Mixer.Buffer.is_partial buf then
                ( self#end_track ; self#begin_track )
      end

  method abort_track =
    self#end_track ;
    self#begin_track (* TODO: this seems to be necessary, i dunno why *) ;
    must_fail <- true

  method wake_up = self#begin_track
  method sleep = self#end_track

end

(** Same thing, with a queue in which we prefetch files,
  * which requests are given by [get_next_request]. *)
class virtual queued =
object (self)
  inherit unqueued as super

  method virtual get_next_request : Request.t option

  (** These are heuristical settings, we should work at it... *)
  method next_resolve_time = 500 (* Frames *)
  method next_timeout = 20. (* Seconds *)

  (** Management of the queue of files waiting to be played. *)

  val mutable polling_thread = Thread.self ()
  val qlock = Tutils.Mutex.create "Play_files.qlock"
  val retrieved = Queue.create ()
  val mutable queue_length = 0 (* Frames *)

  method copy_queue =
    Mutex.lock qlock ;
    let q = Queue.fold (fun l r -> (snd r)::l) [] retrieved in
      Mutex.unlock qlock ;
      q

  (** We are ready if a file is being played,
    * or if there are some files ready to be played in the queue. *)
  method is_ready = super#is_ready || queue_length <> 0

  (** Try to feed the queue with a new request. *)
  method private prefetch =
    log 4 (lazy (Log.f "%s is prefetching ..." id)) ;
    begin
      match self#get_next_request with
        | Some req ->
            ( match Request.resolve req self#next_timeout with
                | Request.Resolved ->
                    let len =
                      match Request.get_metadata req "duration" with
                        | Some f -> (try float_of_string f with _ -> 100.)
                        | None -> 100.
                    in
                    let len = int_of_float
                                (len /. Mixer.Buffer.length)
                    in
                      Mutex.lock qlock ;
                      Queue.add (len,req) retrieved ;
                      log 4 (lazy (Log.f
                                     "%s (rid %d) => queue_len %d+=%d "
                                     id (Request.get_id req)
                                     queue_length len)) ;
                      queue_length <- queue_length + len ;
                      Mutex.unlock qlock ;
                      true
                | Request.Failed (* Failure of resolving or decoding *)
                | Request.Timeout ->
                    Request.destroy req ; false )
        | _ -> false
    end

  (** As long as the queue is not long enough and we succeed in feeding it,
    * keep doing so. Otherwise, sleep a while and try again later. *)
  val mutable sleeping = true
  method private feed_queue =
    let fetch_test =
      log 4 (lazy (Log.f "%s: prefetching ?" id)) ;
      Tutils.mutexify qlock (
        fun () ->
          let r = remaining in
            log 4
              (lazy (Log.f
                       "%s: rem. %03d queue_len. %03d next_resolve %03d"
                       id r queue_length self#next_resolve_time)) ;
            ( self#next_resolve_time = -1 ) ||
            ( r <> -1 &&
              self#next_resolve_time >= queue_length + r )
      )
    in
      while not sleeping do
        while fetch_test () && self#prefetch do () done ;
        Unix.sleep 1 ;
      done

  (** At startup we launch a polling thread for prefetching. *)
  method wake_up =
    sleeping <- false ;
    polling_thread <- ( Tutils.create (fun () -> self#feed_queue) ()
                          (Printf.sprintf "%s queue feeder" id) ) ;
    super#wake_up

  (* At exit, make the polling_thread exit. *)
  method sleep =
    sleeping <- true ;
    super#sleep


  (** Provide the unqueued [super] with resolved requests. *)
  method private get_next_file =
    let ans =
      Mutex.lock qlock ;
      try
        let len,f = Queue.take retrieved in
          log 2 (lazy (Log.f "%s queue_len %d-=%d" id queue_length len)) ;
          queue_length <- queue_length - len ;
          Some f
      with
        | Queue.Empty ->
            log 2 (lazy (Log.f "%s has an empty queue !" id)) ;
            None
    in
      Mutex.unlock qlock ;
      ans

end
