(* What admissions and delivery passes read without a lock: output starts and
   markers, newest first. The streaming clock publishes a new snapshot before
   appending the bytes it points to, so a reader that took the tail first
   keeps only entries at or below it. *)
type snapshot = {
  epoch : int;
  starts : int list;
  stream_start : int option;
  markers : (int * Icy.event) list;
}

(* [lock] serialises the encoder's use with its stop, and the store's
   writes. *)
type t = {
  factory : string -> Frame.Metadata.Export.t -> Encoder.encoder;
  buffer : int;
  burst : int option;
  metaint : int;
  store : Store.t;
  snapshot : snapshot Atomic.t;
  lock : Mutex.t;
  mutable encoder : Encoder.encoder option;
}

type encoded = { data : Strings.t; restart : int option }

let create ~factory ~buffer ~burst ~metaint () =
  {
    factory;
    buffer;
    burst;
    metaint;
    store = Store.create ~retain:buffer ();
    snapshot =
      Atomic.make { epoch = 0; starts = []; stream_start = None; markers = [] };
    lock = Mutex.create ();
    encoder = None;
  }

let newest_start snapshot ~tail =
  match snapshot.starts with start :: _ -> start | [] -> tail

let rec marker_at markers offset =
  match markers with
    | [] -> None
    | (position, event) :: _ when position <= offset -> Some event
    | _ :: rest -> marker_at rest offset

(* Keeps the entries inside the window [N - buffer, T), and the marker in
   effect at its start. *)
let prune t snapshot =
  let window = newest_start snapshot ~tail:max_int - t.buffer in
  let starts = List.filter (fun start -> start >= window) snapshot.starts in
  let rec markers = function
    | [] -> []
    | ((position, _) as marker) :: rest ->
        if position <= window then [marker] else marker :: markers rest
  in
  { snapshot with starts; markers = markers snapshot.markers }

let append t { data; restart } =
  let tail = Store.tail t.store in
  let length = Strings.length data in
  let snapshot = Atomic.get t.snapshot in
  let snapshot =
    match restart with
      | Some offset -> { snapshot with stream_start = Some (tail + offset) }
      | None -> snapshot
  in
  let snapshot =
    if length > 0 then { snapshot with starts = tail :: snapshot.starts }
    else snapshot
  in
  Atomic.set t.snapshot (prune t snapshot);
  Strings.iter
    (fun bytes offset length ->
      Store.append t.store (Bytes.unsafe_of_string bytes) offset length)
    data

let start t ~name ~last =
  Mutex.protect t.lock (fun () ->
      let initial =
        match last with
          | Some (metadata, _) -> metadata
          | None -> Frame.Metadata.Export.empty
      in
      t.encoder <- Some (t.factory name initial);
      let tail = Store.tail t.store in
      Atomic.set t.snapshot
        {
          epoch = tail;
          starts = [];
          stream_start = None;
          markers =
            (match last with Some (_, event) -> [(tail, event)] | None -> []);
        })

let running t = Mutex.protect t.lock (fun () -> t.encoder <> None)

let stop t =
  Mutex.protect t.lock (fun () ->
      match t.encoder with
        | None -> ()
        | Some encoder ->
            t.encoder <- None;
            let data = encoder.Encoder.stop () in
            append t { data; restart = encoder.Encoder.restart_offset () })

let encode t frame =
  Mutex.protect t.lock (fun () ->
      match t.encoder with
        | None -> { data = Strings.empty; restart = None }
        | Some encoder ->
            let data = encoder.Encoder.encode frame in
            { data; restart = encoder.Encoder.restart_offset () })

let send t encoded = Mutex.protect t.lock (fun () -> append t encoded)

let metadata t metadata event =
  Mutex.protect t.lock (fun () ->
      let snapshot = Atomic.get t.snapshot in
      Atomic.set t.snapshot
        {
          snapshot with
          markers = (Store.tail t.store, event) :: snapshot.markers;
        };
      match t.encoder with
        | Some encoder -> encoder.Encoder.encode_metadata metadata
        | None -> ())

(* The encoder's first byte is a candidate only when it reports a stream start
   there: its first output may begin with header pages it does not report. *)
let join_offset t snapshot ~tail =
  match t.burst with
    | None -> tail
    | Some burst -> (
        let window = newest_start snapshot ~tail - t.buffer in
        let lowest, stream_start =
          match snapshot.stream_start with
            | Some start -> (max snapshot.epoch start, [start])
            | None -> (snapshot.epoch, [])
        in
        let candidate offset =
          offset >= lowest && offset >= window && offset <= tail
          && (offset <> snapshot.epoch
             || snapshot.stream_start = Some snapshot.epoch)
        in
        let candidates =
          List.sort_uniq
            (fun a b -> compare b a)
            (List.filter candidate (stream_start @ snapshot.starts))
        in
        match List.find_opt (fun c -> c <= tail - burst) candidates with
          | Some offset -> offset
          | None -> (
              match List.rev candidates with
                | oldest :: _ -> oldest
                | [] -> tail))

(* [carried] holds the unsent initial bytes, or the rest of a block of which
   only part was accepted; it goes out before any further stream byte. *)
type listener = {
  base : Listener.t;
  icy : bool;
  mutable offset : int;
  mutable carried : string;
  mutable carried_offset : int;
  mutable position : Icy.position;
}

type segment = Carried of int | Block of string * Icy.position | Stream of int

let overflow listener =
  (Listener.log listener.base)#info
    "Listener %s buffer overflow (client too slow), disconnecting"
    (Listener.info listener.base).peer;
  Listener.mark_closed listener.base

let pending t listener =
  listener.carried_offset < String.length listener.carried
  || listener.offset < Store.tail t.store

let commit listener segments accepted =
  let rec go accepted = function
    | [] -> ()
    | _ when accepted = 0 -> ()
    | Carried length :: rest ->
        let sent = min accepted length in
        listener.carried_offset <- listener.carried_offset + sent;
        if listener.carried_offset >= String.length listener.carried then (
          listener.carried <- "";
          listener.carried_offset <- 0);
        go (accepted - sent) rest
    | Block (block, position) :: rest ->
        let length = String.length block in
        listener.position <- position;
        if accepted >= length then go (accepted - length) rest
        else (
          listener.carried <- String.sub block accepted (length - accepted);
          listener.carried_offset <- 0)
    | Stream length :: rest ->
        let sent = min accepted length in
        listener.offset <- listener.offset + sent;
        listener.position <- Icy.advance listener.position sent;
        go (accepted - sent) rest
  in
  go accepted segments

let flush t listener ~scratch =
  let tail = Store.tail t.store in
  let snapshot = Atomic.get t.snapshot in
  if listener.offset < newest_start snapshot ~tail - t.buffer then
    overflow listener
  else (
    let capacity = Bytes.length scratch in
    let carried = String.length listener.carried - listener.carried_offset in
    let first = min carried capacity in
    Bytes.blit_string listener.carried listener.carried_offset scratch 0 first;
    let rec stage ~fill ~offset ~position ~valid segments =
      if fill >= capacity || offset >= tail then (fill, valid, segments)
      else if listener.icy && Icy.room ~metaint:t.metaint position = 0 then (
        let block, after =
          Icy.next_block position (marker_at snapshot.markers offset)
        in
        let length = String.length block in
        if fill + length >= capacity then (fill, valid, segments)
        else (
          Bytes.blit_string block 0 scratch fill length;
          stage ~fill:(fill + length) ~offset ~position:after ~valid
            (Block (block, after) :: segments)))
      else (
        let length = min (capacity - fill) (tail - offset) in
        let length =
          if listener.icy then min length (Icy.room ~metaint:t.metaint position)
          else length
        in
        let valid = Store.read t.store ~offset ~length scratch fill && valid in
        stage ~fill:(fill + length) ~offset:(offset + length)
          ~position:(Icy.advance position length)
          ~valid
          (Stream length :: segments))
    in
    let fill, valid, segments =
      if first < carried then (first, true, [])
      else
        stage ~fill:first ~offset:listener.offset ~position:listener.position
          ~valid:true []
    in
    let segments =
      (if first > 0 then [Carried first] else []) @ List.rev segments
    in
    if not valid then overflow listener
    else if fill > 0 then (
      match Listener.write listener.base scratch 0 fill with
        | -1 -> Listener.mark_closed listener.base
        | accepted -> commit listener segments accepted))

let join t ~head ~icy base make =
  Mutex.protect t.lock (fun () ->
      match t.encoder with
        | None -> None
        | Some encoder ->
            let tail = Store.tail t.store in
            let snapshot = Atomic.get t.snapshot in
            let offset = join_offset t snapshot ~tail in
            let header =
              if Some offset = snapshot.stream_start then ""
              else Strings.to_string (encoder.Encoder.header ())
            in
            let header, position =
              if icy then
                Icy.interleave ~metaint:t.metaint Icy.start
                  (marker_at snapshot.markers offset)
                  header
              else (header, Icy.start)
            in
            let listener =
              {
                base;
                icy;
                offset;
                carried = head ^ header;
                carried_offset = 0;
                position;
              }
            in
            Some
              (make
                 ~pending:(fun () -> pending t listener)
                 ~flush:(fun ~scratch -> flush t listener ~scratch)))
