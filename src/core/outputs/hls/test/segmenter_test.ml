(* Conformance checks of spec/segmenting.md, against an encoder model: one
   tick is a millisecond, chunks are 40 ms. *)

open Hls_output

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let tps = 1000
let chunk = 40
let config ~duration = { Segmenter.duration; ticks_per_second = tps }

type encoder =
  (* A split point at every chunk start (packed audio). *)
  | Free_split
  (* Free-split, with split points at these codec frame times. *)
  | Codec_frames of int list
  (* Key packets at these media times, each recorded [delay] ticks after its
     time (interleaving, encoder delay). *)
  | Keyframes of { times : int list; delay : int }

type segment = { start : int; length : int; discontinuous : bool }

let run ?(syncs = []) ?(restarts = []) ~duration ~until encoder =
  let closed = ref [] in
  let seg =
    Segmenter.create
      ~on_open:(fun ~start:_ -> ((), Strings.empty))
      ~on_close:(fun (c : unit Segmenter.closed) ->
        if c.publishable then
          closed :=
            {
              start = c.start;
              length = c.length;
              discontinuous = c.discontinuous;
            }
            :: !closed)
      (config ~duration)
  in
  let pending =
    ref
      (match encoder with
        | Keyframes k -> k.times
        | Codec_frames times -> times
        | Free_split -> [])
  in
  let cadence, delay =
    match encoder with
      | Keyframes k -> (`Key_frames, k.delay)
      | Free_split | Codec_frames _ -> (`Grid, 0)
  in
  while Segmenter.media_time seg < until do
    let before = Segmenter.media_time seg in
    let after = before + chunk in
    List.iter
      (fun (time, discontinuous) ->
        if time = before then Segmenter.sync seg ~time ~discontinuous)
      syncs;
    Segmenter.ensure_open seg;
    Segmenter.advance seg chunk;
    if List.mem before restarts then (
      Segmenter.restart seg ~time:before;
      Segmenter.add_bytes seg (Strings.of_string "x"))
    else (
      match encoder with
        | Free_split ->
            Segmenter.split seg ~cadence ~time:before;
            Segmenter.add_bytes seg (Strings.of_string "x")
        | Codec_frames _ | Keyframes _ ->
            let rec emit () =
              match !pending with
                | k :: rest when k + delay < after ->
                    pending := rest;
                    Segmenter.add_bytes seg (Strings.of_string "x");
                    Segmenter.split seg ~cadence ~time:k;
                    emit ()
                | _ -> ()
            in
            emit ();
            Segmenter.add_bytes seg (Strings.of_string "x"))
  done;
  List.rev !closed

let every ~offset ~step ~until =
  List.init (((until - offset) / step) + 1) (fun i -> offset + (i * step))

let rounded ticks = int_of_float (Float.round (float ticks /. float tps))

(* §4.2 worked example and KC3: key frames 0.1 s after every 2 s. *)
let () =
  let until = 60_000 in
  let segs =
    run ~duration:6000 ~until
      (Keyframes { times = every ~offset:2100 ~step:2000 ~until; delay = 0 })
  in
  let starts = List.map (fun s -> s.start) segs in
  check "worked example starts"
    (List.filteri (fun i _ -> i < 3) starts = [0; 6100; 12100]);
  List.iteri
    (fun i s ->
      if i > 0 then check "KC3 every later segment is 6 s" (s.length = 6000))
    segs

(* KC3: key frames a few frames before every boundary. *)
let () =
  let until = 60_000 in
  let segs =
    run ~duration:6000 ~until
      (Keyframes { times = every ~offset:1880 ~step:2000 ~until; delay = 0 })
  in
  List.iteri
    (fun i s ->
      if i > 0 then check "KC3 early key frames: 6 s" (s.length = 6000))
    segs;
  check "KC3 early key frames: count" (List.length segs >= 8)

(* KC4: a free-split variant of the same output starts segment n within
   0.1 s of the copied one. *)
let () =
  let until = 60_000 in
  let video =
    run ~duration:6000 ~until
      (Keyframes { times = every ~offset:2100 ~step:2000 ~until; delay = 0 })
  in
  let audio = run ~duration:6000 ~until Free_split in
  check "KC4 free-split cuts on the grid"
    (List.for_all (fun s -> s.start mod 6000 = 0) audio);
  List.iteri
    (fun i (v : segment) ->
      match List.nth_opt audio i with
        | Some a -> check "KC4 aligned starts" (abs (v.start - a.start) <= 100)
        | None -> ())
    video

(* KC2: codec frames of 46.44 ms against 40 ms chunks; the grid does not
   drift. *)
let () =
  let until = 120_000 in
  let codec_frames = List.init (until * 100 / 4644) (fun i -> i * 4644 / 100) in
  let segs = run ~duration:2000 ~until (Codec_frames (List.tl codec_frames)) in
  let sum = ref 0 in
  List.iteri
    (fun n s ->
      sum := !sum + s.length;
      check "KC2 sum of n segments within one codec frame of n x D"
        (abs (!sum - ((n + 1) * 2000)) <= 47))
    segs;
  check "KC2 fifty segments" (List.length segs >= 50)

(* §4.2, #5654: key frames every 2.002 s whose packets are recorded 0.9 s
   late, 30 s segments. Every segment is 15 key-frame intervals, whatever the
   delay. *)
let issue_cuts delay =
  let until = 400_000 in
  List.map
    (fun s -> (s.start, s.length))
    (run ~duration:30_000 ~until
       (Keyframes { times = every ~offset:100 ~step:2002 ~until; delay }))

let () =
  let cuts = issue_cuts 900 in
  List.iteri
    (fun i (_, length) ->
      if i > 0 then check "late key packets: 30.03 s segments" (length = 30_030))
    cuts;
  check "late key packets: count" (List.length cuts >= 12);
  check "cuts do not depend on the delay"
    (cuts = issue_cuts 0 && cuts = issue_cuts 5000)

(* §4.2: NTSC key frames never drift away from the cadence. *)
let () =
  let until = 2_000_000 in
  let segs =
    run ~duration:30_000 ~until
      (Keyframes { times = every ~offset:2002 ~step:2002 ~until; delay = 0 })
  in
  check "NTSC: every segment 30.03 s"
    (List.for_all (fun s -> s.length = 30_030) segs);
  check "NTSC: count" (List.length segs >= 60)

(* §4.2: key frames just before the target. *)
let () =
  let until = 400_000 in
  let segs =
    run ~duration:30_000 ~until
      (Keyframes { times = every ~offset:29_996 ~step:29_996 ~until; delay = 0 })
  in
  check "key frames before the target: 29.996 s"
    (List.for_all (fun s -> s.length = 29_996) segs);
  check "key frames before the target: count" (List.length segs >= 10)

(* §4.2: a key-frame interval that does not divide D gives the nearest
   eligible length, steadily. *)
let () =
  let until = 60_000 in
  let segs =
    run ~duration:6000 ~until
      (Keyframes { times = every ~offset:2500 ~step:2500 ~until; delay = 0 })
  in
  check "2.5 s key frames, 6 s segments: 5 s"
    (List.for_all (fun s -> s.length = 5000) segs)

(* §4.2: key frames sparser than T: cuts only at key frames, each segment
   one interval. *)
let () =
  let until = 40_000 in
  let segs =
    run ~duration:1000 ~until
      (Keyframes { times = every ~offset:4000 ~step:4000 ~until; delay = 0 })
  in
  check "sparse key frames: cut only at key frames"
    (List.for_all (fun s -> s.start mod 4000 = 0 && s.length = 4000) segs)

(* KC5: a segment is never of length 0 nor empty; the first starts at 0. *)
let () =
  let segs = run ~duration:2000 ~until:10_000 Free_split in
  check "KC5 first at 0" ((List.hd segs).start = 0);
  check "KC5 positive lengths" (List.for_all (fun s -> s.length > 0) segs)

(* KC6, §7: a sync cut lands at its time in free-split variants and does not
   move the grid. *)
let () =
  let segs =
    run ~syncs:[(7000, false)] ~duration:6000 ~until:30_000 Free_split
  in
  let starts = List.map (fun s -> s.start) segs in
  check "KC6 sync cut and unchanged grid"
    (starts = [0; 6000; 7000; 12000; 18000])

(* §7: a sync cut waits for the next split point in a copied variant. *)
let () =
  let until = 30_000 in
  let segs =
    run
      ~syncs:[(7000, true)]
      ~duration:6000 ~until
      (Keyframes { times = every ~offset:2100 ~step:2000 ~until; delay = 0 })
  in
  match List.find_opt (fun s -> s.discontinuous) segs with
    | Some s -> check "sync waits for the split point" (s.start = 8100)
    | None -> check "discontinuous sync cut made" false

(* §8: a restart cuts at its time, discontinuous; boundaries after it stay on
   the grid. *)
let () =
  let segs = run ~restarts:[7000] ~duration:6000 ~until:30_000 Free_split in
  let starts = List.map (fun s -> (s.start, s.discontinuous)) segs in
  check "restart"
    (starts
    = [(0, false); (6000, false); (7000, true); (12000, false); (18000, false)]
    )

(* §7: a sync at the time of the variant's own restart is that cut. *)
let () =
  let segs =
    run
      ~syncs:[(7040, true)]
      ~restarts:[7000] ~duration:6000 ~until:20_000 Free_split
  in
  check "late sync after restart cuts once more"
    (List.length (List.filter (fun s -> s.discontinuous) segs) = 2);
  let segs =
    run
      ~syncs:[(7000, true)]
      ~restarts:[7000] ~duration:6000 ~until:20_000 Free_split
  in
  check "same-time sync and restart: one cut"
    (List.length (List.filter (fun s -> s.discontinuous) segs) = 1)

let () =
  check "T rounds 6.006 s to 6"
    (Segmenter.target_duration { duration = 6006; ticks_per_second = tps } = 6);
  check "T at least 1"
    (Segmenter.target_duration { duration = 400; ticks_per_second = tps } = 1);
  Printf.printf "segmenter: %d checks passed\n" !checks;
  if !checks < 20 then failwith "too few checks ran"
