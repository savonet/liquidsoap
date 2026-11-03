type kind = [ `Initial | `Preroll | `Stream | `Block ]

(* Stream bytes are copied into chunks the listener reuses: a chunk returns to
   its pool once no piece refers to it. *)
type chunk = { bytes : bytes; mutable refs : int }

(* A piece is the slice [start, stop) of [data], so a preroll shared by many
   listeners is never copied. [output] numbers the encoder output a stream
   piece comes from; only stream pieces of outputs older than [newest] count
   towards overflow. *)
type piece = {
  kind : kind;
  data : [ `String of string | `Chunk of chunk ];
  start : int;
  stop : int;
  mutable sent : int;
  output : int;
}

(* [encoder] is [None] once stopped, under [encoder_lock]; the queue and the
   overflow counters are under [queue_lock], which is never held during an
   encode or a socket write. *)
type listener = {
  base : Listener.t;
  icy : bool;
  encoder_lock : Mutex.t;
  mutable encoder : Encoder.encoder option;
  queue_lock : Mutex.t;
  queue : piece Queue.t;
  mutable prerolling : int;
  mutable position : Icy.position;
  mutable newest : int;
  mutable newest_owed : int;
  mutable older_owed : int;
  mutable tail : (chunk * int) option;
  mutable free : chunk list;
}

type t = {
  factory : string -> Frame.Metadata.Export.t -> Encoder.encoder;
  buffer : int;
  metaint : int;
  lock : Mutex.t;
  mutable name : string option;
  mutable last : (Frame.Metadata.Export.t * Icy.event) option;
  mutable listeners : listener list;
}

let create ~factory ~buffer ~metaint () =
  {
    factory;
    buffer;
    metaint;
    lock = Mutex.create ();
    name = None;
    last = None;
    listeners = [];
  }

let start t ~name ~last =
  Mutex.protect t.lock (fun () ->
      t.name <- Some name;
      t.last <- last)

let running t = Mutex.protect t.lock (fun () -> t.name <> None)
let stamp t = Mutex.protect t.lock (fun () -> Option.map snd t.last)
let chunk_size = 65536

let push listener ~kind ~output ~start ~stop data =
  if stop > start then (
    (match data with `Chunk chunk -> chunk.refs <- chunk.refs + 1 | _ -> ());
    Queue.push { kind; data; start; stop; sent = start; output } listener.queue)

let release listener = function
  | `Chunk chunk ->
      chunk.refs <- chunk.refs - 1;
      let is_tail =
        match listener.tail with Some (c, _) -> c == chunk | None -> false
      in
      if chunk.refs = 0 && not is_tail then
        listener.free <- chunk :: listener.free
  | `String _ -> ()

(* Opens the next encoder output. Called with [queue_lock] held. *)
let open_output listener ~kind =
  if kind = `Stream then (
    listener.older_owed <- listener.older_owed + listener.newest_owed;
    listener.newest_owed <- 0;
    listener.newest <- listener.newest + 1)

(* Queues [start, stop) of [data], with ICY blocks where the position requires
   them. Called with [queue_lock] held. *)
let enqueue t listener ~kind ~stamp ~start ~stop data =
  let output = listener.newest in
  let rec go offset =
    if offset < stop then
      if listener.icy && Icy.room ~metaint:t.metaint listener.position = 0 then (
        let block, position = Icy.next_block listener.position stamp in
        push listener ~kind:`Block ~output ~start:0 ~stop:(String.length block)
          (`String block);
        listener.position <- position;
        go offset)
      else (
        let length = stop - offset in
        let length =
          if listener.icy then
            min length (Icy.room ~metaint:t.metaint listener.position)
          else length
        in
        push listener ~kind ~output ~start:offset ~stop:(offset + length) data;
        if kind = `Stream then
          listener.newest_owed <- listener.newest_owed + length;
        listener.position <- Icy.advance listener.position length;
        go (offset + length))
  in
  go start

let enqueue_string t listener ~kind ~stamp data =
  open_output listener ~kind;
  enqueue t listener ~kind ~stamp ~start:0 ~stop:(String.length data)
    (`String data)

(* The chunk to fill next and its fill: the tail, emptied when nothing refers to
   it any more, or a free chunk. *)
let next_chunk listener =
  match listener.tail with
    | Some (chunk, _) when chunk.refs = 0 -> (chunk, 0)
    | Some (chunk, fill) when fill < chunk_size -> (chunk, fill)
    | _ -> (
        match listener.free with
          | chunk :: rest ->
              listener.free <- rest;
              (chunk, 0)
          | [] -> ({ bytes = Bytes.create chunk_size; refs = 0 }, 0))

(* Copies an encoder output into the listener's chunks and queues it. Called
   with [queue_lock] held. *)
let enqueue_output t listener ~stamp { Encoder.length; blit } =
  open_output listener ~kind:`Stream;
  let rec go offset =
    if offset < length then (
      let chunk, fill = next_chunk listener in
      (match listener.tail with
        | Some (previous, _) when previous != chunk && previous.refs = 0 ->
            listener.free <- previous :: listener.free
        | _ -> ());
      let n = min (length - offset) (chunk_size - fill) in
      blit offset chunk.bytes fill n;
      listener.tail <- Some (chunk, fill + n);
      enqueue t listener ~kind:`Stream ~stamp ~start:fill ~stop:(fill + n)
        (`Chunk chunk);
      go (offset + n))
  in
  go 0

let check_overflow listener overflow =
  if overflow then Listener.close_for_overflow listener.base

let append t listener ~stamp data =
  check_overflow listener
    (Mutex.protect listener.queue_lock (fun () ->
         enqueue_string t listener ~kind:`Stream ~stamp (Strings.to_string data);
         listener.older_owed > t.buffer))

let append_output t listener ~stamp output =
  check_overflow listener
    (Mutex.protect listener.queue_lock (fun () ->
         enqueue_output t listener ~stamp output;
         listener.older_owed > t.buffer))

(* Runs [f] on the listener's encoder unless it is stopped or the listener is
   closed: the closed flag is set before teardown stops the encoder, so a
   closed listener's encoder is never used. *)
let with_encoder listener f =
  Mutex.protect listener.encoder_lock (fun () ->
      match listener.encoder with
        | Some encoder when not (Listener.is_closed listener.base) ->
            Some (f encoder)
        | _ -> None)

let stop_encoder listener =
  Mutex.protect listener.encoder_lock (fun () ->
      match listener.encoder with
        | None -> None
        | Some encoder ->
            listener.encoder <- None;
            Some (encoder.Encoder.stop ()))

let live t =
  Mutex.protect t.lock (fun () ->
      t.listeners <-
        List.filter (fun l -> not (Listener.is_closed l.base)) t.listeners;
      t.listeners)

let stop t =
  let listeners =
    Mutex.protect t.lock (fun () ->
        t.name <- None;
        let listeners = t.listeners in
        t.listeners <- [];
        listeners)
  in
  let stamp = stamp t in
  List.iter
    (fun listener ->
      match stop_encoder listener with
        | Some data -> append t listener ~stamp data
        | None -> ())
    listeners

let prerolling listener =
  Mutex.protect listener.queue_lock (fun () -> listener.prerolling > 0)

(* A listener still owed its preroll gets no piece: its stream starts once the
   preroll is sent, so the encoder output does not pile up behind it. *)
let send_one t ~stamp frame listener =
  if not (prerolling listener) then
    ignore
      (with_encoder listener (fun e ->
           match e.Encoder.encode_into with
             | Some encode_into ->
                 encode_into frame (fun output ->
                     if output.Encoder.length > 0 then
                       append_output t listener ~stamp output)
             | None ->
                 let data = e.Encoder.encode frame in
                 if not (Strings.is_empty data) then
                   append t listener ~stamp data))

(* ponytail: fixed batch size, tune from measurements if dispatch dominates. *)
let batch_size = 32

let rec batches = function
  | [] -> []
  | l ->
      let rec split n acc = function
        | x :: rest when n > 0 -> split (n - 1) (x :: acc) rest
        | rest -> (List.rev acc, rest)
      in
      let batch, rest = split batch_size [] l in
      batch :: batches rest

(* Each batch of listeners is encoded by a task of its own, and the clock parks
   until all are done, so the cycle's send ends with every listener served. A
   listener is in one batch per cycle, so its pieces stay in order. *)
let send t frame =
  let stamp = stamp t in
  match batches (live t) with
    | [] -> ()
    | [batch] -> List.iter (send_one t ~stamp frame) batch
    | batches ->
        let remaining = Atomic.make (List.length batches) in
        let failure = Atomic.make None in
        let finished = Scheduler.Condition.create () in
        List.iter
          (fun batch ->
            Scheduler.Task.add
              {
                Scheduler.Task.priority = `Blocking;
                events = [`Delay 0.];
                handler =
                  (fun _ ->
                    (try List.iter (send_one t ~stamp frame) batch
                     with exn ->
                       let bt = Printexc.get_raw_backtrace () in
                       ignore
                         (Atomic.compare_and_set failure None (Some (exn, bt))));
                    Atomic.decr remaining;
                    Scheduler.Condition.signal finished;
                    []);
              })
          batches;
        Duppy.Condition.wait ~priority:`Clock Scheduler.raw finished (fun () ->
            Atomic.get remaining = 0);
        Option.iter
          (fun (exn, bt) -> Printexc.raise_with_backtrace exn bt)
          (Atomic.get failure)

let metadata t metadata event =
  Mutex.protect t.lock (fun () -> t.last <- Some (metadata, event));
  List.iter
    (fun listener ->
      ignore
        (with_encoder listener (fun e -> e.Encoder.encode_metadata metadata)))
    (live t)

let pending listener =
  Mutex.protect listener.queue_lock (fun () ->
      not (Queue.is_empty listener.queue))

(* Copies the front of the queue into [scratch]: only flush drops pieces, so
   they stay in place while the lock is released for the write. *)
let stage listener scratch =
  Mutex.protect listener.queue_lock (fun () ->
      let capacity = Bytes.length scratch in
      let rec go fill pieces =
        match pieces () with
          | Seq.Cons (piece, rest) when fill < capacity ->
              let length = min (piece.stop - piece.sent) (capacity - fill) in
              (match piece.data with
                | `String data ->
                    Bytes.blit_string data piece.sent scratch fill length
                | `Chunk chunk ->
                    Bytes.blit chunk.bytes piece.sent scratch fill length);
              go (fill + length) rest
          | _ -> fill
      in
      go 0 (Queue.to_seq listener.queue))

let drop listener accepted =
  Mutex.protect listener.queue_lock (fun () ->
      let rec go accepted =
        if accepted > 0 && not (Queue.is_empty listener.queue) then (
          let piece = Queue.peek listener.queue in
          let sent = min accepted (piece.stop - piece.sent) in
          piece.sent <- piece.sent + sent;
          if piece.kind = `Stream then
            if piece.output = listener.newest then
              listener.newest_owed <- listener.newest_owed - sent
            else listener.older_owed <- listener.older_owed - sent;
          if piece.kind = `Preroll then
            listener.prerolling <- listener.prerolling - sent;
          if piece.sent = piece.stop then (
            ignore (Queue.pop listener.queue);
            release listener piece.data);
          go (accepted - sent))
      in
      go accepted)

(* ponytail: fixed threshold; below it, staging coalesces small pieces. *)
let direct_write = 8192

(* A large chunk piece at the front of the queue, written from its chunk with
   no staging copy. Only flush drops pieces, so it stays in place while the
   lock is released for the write. *)
let front_chunk listener =
  Mutex.protect listener.queue_lock (fun () ->
      match Queue.peek_opt listener.queue with
        | Some { data = `Chunk chunk; sent; stop; _ }
          when stop - sent >= direct_write ->
            Some (chunk.bytes, sent, stop - sent)
        | _ -> None)

let write listener bytes offset length =
  match Listener.write listener.base bytes offset length with
    | -1 -> Listener.mark_closed listener.base
    | accepted -> drop listener accepted

let flush listener ~scratch =
  match front_chunk listener with
    | Some (bytes, offset, length) -> write listener bytes offset length
    | None ->
        let fill = stage listener scratch in
        if fill > 0 then write listener scratch 0 fill

let teardown listener = ignore (stop_encoder listener)

let join t ~head ~icy ~preroll base make =
  match
    Mutex.protect t.lock (fun () -> Option.map (fun n -> (n, t.last)) t.name)
  with
    | None -> None
    | Some (name, last) ->
        let initial =
          match last with
            | Some (metadata, _) -> metadata
            | None -> Frame.Metadata.Export.empty
        in
        let encoder = t.factory name initial in
        let listener =
          {
            base;
            icy;
            encoder_lock = Mutex.create ();
            encoder = Some encoder;
            queue_lock = Mutex.create ();
            queue = Queue.create ();
            prerolling = 0;
            position = Icy.start;
            newest = 0;
            newest_owed = 0;
            older_owed = 0;
            tail = None;
            free = [];
          }
        in
        let header = Strings.to_string (encoder.Encoder.header ()) in
        Mutex.protect listener.queue_lock (fun () ->
            push listener ~kind:`Initial ~output:0 ~start:0
              ~stop:(String.length head) (`String head);
            (match preroll with
              | Some data ->
                  listener.prerolling <- String.length data;
                  enqueue_string t listener ~kind:`Preroll ~stamp:None data
              | None -> ());
            enqueue_string t listener ~kind:`Initial
              ~stamp:(Option.map snd last) header);
        let registered =
          Mutex.protect t.lock (fun () ->
              if t.name = None then false
              else (
                t.listeners <- listener :: t.listeners;
                true))
        in
        if not registered then (
          ignore (encoder.Encoder.stop ());
          None)
        else
          Some
            (make
               ~pending:(fun () -> pending listener)
               ~flush:(fun ~scratch -> flush listener ~scratch)
               ~teardown:(fun () -> teardown listener))
