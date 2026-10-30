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
let join_offset t snapshot ~tail ~burst =
  match burst with
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

(* A preroll owed before the listener joins the stream, with the header and
   epoch of the encoder at admission: encoder code never runs in a delivery
   pass, where the join happens. *)
type preroll = {
  data : string;
  mutable sent : int;
  header : string;
  epoch : int;
}

(* [carried] holds the unsent initial bytes, or the rest of a block of which
   only part was accepted; it goes out before any further stream byte.
   [offset] is set when the listener joins, once its preroll is sent. *)
type listener = {
  base : Listener.t;
  icy : bool;
  mutable offset : int;
  mutable carried : string;
  mutable carried_offset : int;
  mutable position : Icy.position;
  mutable preroll : preroll option;
}

type segment =
  | Carried of int
  | Block of string * Icy.position
  | Stream of int
  | Preroll of int

let pending t listener =
  listener.carried_offset < String.length listener.carried
  || Option.is_some listener.preroll
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
    | Preroll length :: rest ->
        let sent = min accepted length in
        Option.iter
          (fun preroll -> preroll.sent <- preroll.sent + sent)
          listener.preroll;
        listener.position <- Icy.advance listener.position sent;
        go (accepted - sent) rest
  in
  go accepted segments

(* The join point and header of a listener whose preroll is sent, its burst
   shortened by the preroll as Icecast does with intros: [None] when the
   encoder restarted since admission and the stream needs a header that is
   no longer known. *)
let join_point t preroll =
  let tail = Store.tail t.store in
  let snapshot = Atomic.get t.snapshot in
  let burst =
    Option.map (fun burst -> max 0 (burst - String.length preroll.data)) t.burst
  in
  let offset = join_offset t snapshot ~tail ~burst in
  if Some offset = snapshot.stream_start then Some (offset, "", snapshot)
  else if snapshot.epoch = preroll.epoch || preroll.header = "" then
    Some (offset, preroll.header, snapshot)
  else None

let finish_preroll t listener preroll =
  match join_point t preroll with
    | None ->
        (Listener.log listener.base)#info
          "Listener %s: encoder restarted during its preroll, disconnecting"
          (Listener.info listener.base).peer;
        Listener.mark_closed listener.base
    | Some (offset, header, snapshot) ->
        let header, position =
          if listener.icy then
            Icy.interleave ~metaint:t.metaint listener.position
              (marker_at snapshot.markers offset)
              header
          else (header, listener.position)
        in
        listener.offset <- offset;
        listener.carried <- header;
        listener.carried_offset <- 0;
        listener.position <- position;
        listener.preroll <- None

(* Offers the carried bytes then the preroll, with empty ICY blocks where the
   position requires them, and joins the stream once all of it is accepted. *)
let flush_preroll t listener preroll ~scratch =
  let capacity = Bytes.length scratch in
  let carried = String.length listener.carried - listener.carried_offset in
  let first = min carried capacity in
  Bytes.blit_string listener.carried listener.carried_offset scratch 0 first;
  let total = String.length preroll.data in
  let rec stage ~fill ~sent ~position segments =
    if fill >= capacity || sent >= total then (fill, segments)
    else if listener.icy && Icy.room ~metaint:t.metaint position = 0 then (
      let block, after = Icy.next_block position None in
      let length = String.length block in
      if fill + length >= capacity then (fill, segments)
      else (
        Bytes.blit_string block 0 scratch fill length;
        stage ~fill:(fill + length) ~sent ~position:after
          (Block (block, after) :: segments)))
    else (
      let length = min (capacity - fill) (total - sent) in
      let length =
        if listener.icy then min length (Icy.room ~metaint:t.metaint position)
        else length
      in
      Bytes.blit_string preroll.data sent scratch fill length;
      stage ~fill:(fill + length) ~sent:(sent + length)
        ~position:(Icy.advance position length)
        (Preroll length :: segments))
  in
  let fill, segments =
    if first < carried then (first, [])
    else stage ~fill:first ~sent:preroll.sent ~position:listener.position []
  in
  let segments =
    (if first > 0 then [Carried first] else []) @ List.rev segments
  in
  let accepted =
    if fill = 0 then 0 else Listener.write listener.base scratch 0 fill
  in
  if accepted = -1 then Listener.mark_closed listener.base
  else (
    commit listener segments accepted;
    if
      preroll.sent >= total
      && listener.carried_offset >= String.length listener.carried
    then finish_preroll t listener preroll)

let flush_stream t listener ~scratch =
  let tail = Store.tail t.store in
  let snapshot = Atomic.get t.snapshot in
  if listener.offset < newest_start snapshot ~tail - t.buffer then
    Listener.close_for_overflow listener.base
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
    if not valid then Listener.close_for_overflow listener.base
    else if fill > 0 then (
      match Listener.write listener.base scratch 0 fill with
        | -1 -> Listener.mark_closed listener.base
        | accepted -> commit listener segments accepted))

let flush t listener ~scratch =
  match listener.preroll with
    | Some preroll -> flush_preroll t listener preroll ~scratch
    | None -> flush_stream t listener ~scratch

let join t ~head ~icy ~preroll base make =
  Mutex.protect t.lock (fun () ->
      match t.encoder with
        | None -> None
        | Some encoder ->
            let tail = Store.tail t.store in
            let snapshot = Atomic.get t.snapshot in
            let listener =
              match preroll with
                | Some data when data <> "" ->
                    {
                      base;
                      icy;
                      offset = tail;
                      carried = head;
                      carried_offset = 0;
                      position = Icy.start;
                      preroll =
                        Some
                          {
                            data;
                            sent = 0;
                            header =
                              Strings.to_string (encoder.Encoder.header ());
                            epoch = snapshot.epoch;
                          };
                    }
                | _ ->
                    let offset = join_offset t snapshot ~tail ~burst:t.burst in
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
                    {
                      base;
                      icy;
                      offset;
                      carried = head ^ header;
                      carried_offset = 0;
                      position;
                      preroll = None;
                    }
            in
            Some
              (make
                 ~pending:(fun () -> pending t listener)
                 ~flush:(fun ~scratch -> flush t listener ~scratch)
                 ~teardown:ignore))
