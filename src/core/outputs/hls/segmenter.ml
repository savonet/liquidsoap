type config = { duration : int; ticks_per_second : int }

let rounded_seconds { ticks_per_second; _ } ticks =
  ((2 * ticks) + ticks_per_second) / (2 * ticks_per_second)

let target_duration config = max 1 (rounded_seconds config config.duration)

type 'a closed = {
  payload : 'a;
  start : int;
  length : int;
  bytes : Strings.t;
  discontinuous : bool;
  forced : bool;
  publishable : bool;
}

type split_point = { time : int; offset : int; forced : bool }

type 'a segment = {
  payload : 'a;
  start : int;
  prefix_length : int;
  forced : bool;
  mutable discontinuous : bool;
  mutable bytes : Strings.t;
  (* Newest first. *)
  mutable splits : split_point list;
}

type 'a t = {
  config : config;
  target : int;
  on_open : start:int -> 'a * Strings.t;
  on_close : 'a closed -> unit;
  mutable media : int;
  mutable boundary : int;
  mutable segment : 'a segment option;
  (* Increasing times, each with its discontinuity. *)
  mutable syncs : (int * bool) list;
}

let create ~on_open ~on_close config =
  {
    config;
    target = target_duration config;
    on_open;
    on_close;
    media = 0;
    boundary = config.duration;
    segment = None;
    syncs = [];
  }

let media_time t = t.media
let current t = Option.map (fun (s : _ segment) -> s.payload) t.segment

let request t =
  let d = t.config.duration in
  let boundary = max d ((t.media + d - 1) / d * d) in
  match t.syncs with (sync, _) :: _ -> min sync boundary | [] -> boundary

let first_boundary_after t time =
  ((time / t.config.duration) + 1) * t.config.duration

let open_segment t ~start ~discontinuous ~forced ~bytes ~splits =
  let payload, prefix = t.on_open ~start in
  let prefix_length = Strings.length prefix in
  let splits =
    List.map
      (fun (s : split_point) -> { s with offset = s.offset + prefix_length })
      splits
  in
  t.segment <-
    Some
      {
        payload;
        start;
        prefix_length;
        forced;
        discontinuous;
        bytes = Strings.append prefix bytes;
        splits;
      };
  t.boundary <- first_boundary_after t start

let close t (s : _ segment) ~length ~bytes =
  t.segment <- None;
  t.on_close
    {
      payload = s.payload;
      start = s.start;
      length;
      bytes;
      discontinuous = s.discontinuous;
      forced = s.forced;
      publishable = length > 0 && Strings.length bytes > s.prefix_length;
    }

let cut t (s : _ segment) ~discontinuous { time; offset; forced } =
  let kept = Strings.sub s.bytes 0 offset in
  let moved = Strings.drop s.bytes offset in
  let splits =
    List.filter_map
      (fun (p : split_point) ->
        if p.time > time then Some { p with offset = p.offset - offset }
        else None)
      s.splits
  in
  close t s ~length:(time - s.start) ~bytes:kept;
  open_segment t ~start:time ~discontinuous ~forced ~bytes:moved ~splits

let segment t = match t.segment with Some s -> s | None -> assert false

let eligible t (s : _ segment) time =
  rounded_seconds t.config (time - s.start) <= t.target

let rec cut_rule t =
  let s = segment t in
  let k = t.boundary in
  match s.splits with
    | a :: _ when a.time >= k -> (
        let b = List.find_opt (fun (p : split_point) -> p.time <= k) s.splits in
        let point =
          match b with
            | Some b
              when eligible t s b.time
                   && ((not (eligible t s a.time)) || k - b.time <= a.time - k)
              ->
                b
            | _ -> a
        in
        let served =
          point == a && eligible t s a.time && k - s.start < a.time - k
        in
        let next = first_boundary_after t (max k point.time) in
        match served with
          | true -> t.boundary <- next
          | false ->
              cut t s ~discontinuous:false point;
              t.boundary <- next;
              cut_rule t)
    | _ -> ()

let sync_cut t =
  let s = segment t in
  match (s.splits, t.syncs) with
    | latest :: _, (sync, _) :: _ when sync <= latest.time ->
        let due, pending =
          List.partition (fun (sync, _) -> sync <= latest.time) t.syncs
        in
        t.syncs <- pending;
        cut t s ~discontinuous:(List.exists snd due) latest
    | _ -> ()

let rec bound ?(split_due = false) ~length t =
  match t.segment with
    | None ->
        open_segment t ~start:t.media ~discontinuous:false ~forced:false
          ~bytes:Strings.empty ~splits:[];
        bound ~split_due ~length t
    | Some _ when split_due -> false
    | Some s
      when rounded_seconds t.config (t.media + length - s.start) > t.target -> (
        match s.splits with
          | latest :: _ ->
              cut t s ~discontinuous:false latest;
              bound ~split_due ~length t
          | [] -> true)
    | Some _ -> false

let advance t length = t.media <- t.media + length

let add_bytes t bytes =
  let s = segment t in
  s.bytes <- Strings.append s.bytes bytes

let split t ~time ~forced =
  let s = segment t in
  if time > s.start then (
    s.splits <- { time; offset = Strings.length s.bytes; forced } :: s.splits;
    sync_cut t;
    cut_rule t)

let restart t ~time =
  let s = segment t in
  t.syncs <- List.filter (fun (sync, _) -> sync > time) t.syncs;
  cut t s ~discontinuous:true
    { time; offset = Strings.length s.bytes; forced = false }

let sync t ~time ~discontinuous =
  match t.segment with
    | None -> ()
    | Some s when s.start >= time ->
        s.discontinuous <- s.discontinuous || discontinuous
    | Some s -> (
        match List.rev (List.filter (fun p -> p.time >= time) s.splits) with
          | first :: _ -> cut t s ~discontinuous first
          | [] -> t.syncs <- List.sort compare ((time, discontinuous) :: t.syncs)
        )

let finish t =
  match t.segment with
    | None -> ()
    | Some s -> close t s ~length:(t.media - s.start) ~bytes:s.bytes

let abort t =
  let payload = current t in
  t.segment <- None;
  payload
