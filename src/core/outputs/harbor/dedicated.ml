type kind = [ `Initial | `Stream | `Block ]

(* [output] numbers the encoder output a stream piece comes from; only stream
   pieces of outputs older than [newest] count towards overflow. *)
type piece = { kind : kind; data : string; mutable sent : int; output : int }

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
  mutable position : Icy.position;
  mutable newest : int;
  mutable newest_owed : int;
  mutable older_owed : int;
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

let push listener ~kind ~output data =
  if data <> "" then Queue.push { kind; data; sent = 0; output } listener.queue

(* Queues [data] as one encoder output, with ICY blocks where the position
   requires them. Called with [queue_lock] held. *)
let enqueue t listener ~kind ~stamp data =
  if kind = `Stream then (
    listener.older_owed <- listener.older_owed + listener.newest_owed;
    listener.newest_owed <- 0;
    listener.newest <- listener.newest + 1);
  let output = listener.newest in
  let rec go offset =
    if offset < String.length data then
      if listener.icy && Icy.room ~metaint:t.metaint listener.position = 0 then (
        let block, position = Icy.next_block listener.position stamp in
        push listener ~kind:`Block ~output block;
        listener.position <- position;
        go offset)
      else (
        let length = String.length data - offset in
        let length =
          if listener.icy then
            min length (Icy.room ~metaint:t.metaint listener.position)
          else length
        in
        push listener ~kind ~output (String.sub data offset length);
        if kind = `Stream then
          listener.newest_owed <- listener.newest_owed + length;
        listener.position <- Icy.advance listener.position length;
        go (offset + length))
  in
  go 0

let append t listener ~stamp data =
  let overflow =
    Mutex.protect listener.queue_lock (fun () ->
        enqueue t listener ~kind:`Stream ~stamp (Strings.to_string data);
        listener.older_owed > t.buffer)
  in
  if overflow then Listener.close_for_overflow listener.base

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

let send t frame =
  let stamp = stamp t in
  List.iter
    (fun listener ->
      match with_encoder listener (fun e -> e.Encoder.encode frame) with
        | Some data when not (Strings.is_empty data) ->
            append t listener ~stamp data
        | _ -> ())
    (live t)

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
              let length =
                min (String.length piece.data - piece.sent) (capacity - fill)
              in
              Bytes.blit_string piece.data piece.sent scratch fill length;
              go (fill + length) rest
          | _ -> fill
      in
      go 0 (Queue.to_seq listener.queue))

let drop listener accepted =
  Mutex.protect listener.queue_lock (fun () ->
      let rec go accepted =
        if accepted > 0 && not (Queue.is_empty listener.queue) then (
          let piece = Queue.peek listener.queue in
          let sent = min accepted (String.length piece.data - piece.sent) in
          piece.sent <- piece.sent + sent;
          if piece.kind = `Stream then
            if piece.output = listener.newest then
              listener.newest_owed <- listener.newest_owed - sent
            else listener.older_owed <- listener.older_owed - sent;
          if piece.sent = String.length piece.data then
            ignore (Queue.pop listener.queue);
          go (accepted - sent))
      in
      go accepted)

let flush listener ~scratch =
  let fill = stage listener scratch in
  if fill > 0 then (
    match Listener.write listener.base scratch 0 fill with
      | -1 -> Listener.mark_closed listener.base
      | accepted -> drop listener accepted)

let teardown listener = ignore (stop_encoder listener)

let join t ~head ~icy base make =
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
            position = Icy.start;
            newest = 0;
            newest_owed = 0;
            older_owed = 0;
          }
        in
        Mutex.protect listener.queue_lock (fun () ->
            push listener ~kind:`Initial ~output:0 head;
            enqueue t listener ~kind:`Initial ~stamp:(Option.map snd last)
              (Strings.to_string (encoder.Encoder.header ())));
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
