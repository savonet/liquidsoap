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
  | Free_split
  (* Key packets at these media times; [honour_force] says whether a forced
     split is made. *)
  | Keyframes of { times : int list; honour_force : bool }

type segment = {
  start : int;
  length : int;
  discontinuous : bool;
  forced : bool;
}

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
              forced = c.forced;
            }
            :: !closed)
      (config ~duration)
  in
  let pending_keys =
    ref (match encoder with Keyframes k -> k.times | _ -> [])
  in
  while Segmenter.media_time seg < until do
    let before = Segmenter.media_time seg in
    let after = before + chunk in
    List.iter
      (fun (time, discontinuous) ->
        if time = before then Segmenter.sync seg ~time ~discontinuous)
      syncs;
    let force = Segmenter.bound ~length:chunk seg in
    Segmenter.advance seg chunk;
    if List.mem before restarts then (
      Segmenter.restart seg ~time:before;
      Segmenter.add_bytes seg (Strings.of_string "x"))
    else (
      match encoder with
        | Free_split ->
            Segmenter.split seg ~time:before ~forced:force;
            Segmenter.add_bytes seg (Strings.of_string "x")
        | Keyframes { honour_force; _ } ->
            if force && honour_force then
              Segmenter.split seg ~time:before ~forced:true;
            let rec emit () =
              match !pending_keys with
                | k :: rest when k < after ->
                    pending_keys := rest;
                    Segmenter.add_bytes seg (Strings.of_string "x");
                    Segmenter.split seg ~time:k ~forced:false;
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
      (Keyframes
         { times = every ~offset:2100 ~step:2000 ~until; honour_force = true })
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
      (Keyframes
         { times = every ~offset:1880 ~step:2000 ~until; honour_force = true })
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
      (Keyframes
         { times = every ~offset:2100 ~step:2000 ~until; honour_force = true })
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
  let segs =
    run ~duration:2000 ~until
      (Keyframes { times = List.tl codec_frames; honour_force = true })
  in
  let sum = ref 0 in
  List.iteri
    (fun n s ->
      sum := !sum + s.length;
      check "KC2 sum of n segments within one codec frame of n x D"
        (abs (!sum - ((n + 1) * 2000)) <= 47))
    segs;
  check "KC2 fifty segments" (List.length segs >= 50)

(* KC1: key frames every 4 s, 1 s segments. *)
let () =
  let until = 40_000 in
  let segs =
    run ~duration:1000 ~until
      (Keyframes
         { times = every ~offset:4000 ~step:4000 ~until; honour_force = true })
  in
  check "KC1 bound holds" (List.for_all (fun s -> rounded s.length <= 1) segs);
  check "KC1 forced segments exist" (List.exists (fun s -> s.forced) segs);
  check "KC1 key frame segments are not forced"
    (List.for_all (fun s -> s.forced = (s.start mod 4000 <> 0)) segs)

(* KC1: an encoder that declines force: cut at the next split point. *)
let () =
  let until = 40_000 in
  let segs =
    run ~duration:1000 ~until
      (Keyframes
         { times = every ~offset:4000 ~step:4000 ~until; honour_force = false })
  in
  check "KC1 declined force: cut only at split points"
    (List.for_all (fun s -> s.start mod 4000 = 0) segs)

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
      (Keyframes
         { times = every ~offset:2100 ~step:2000 ~until; honour_force = true })
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

(* §5: with a split point due, the bound neither cuts nor forces. *)
let () =
  let closed = ref 0 in
  let seg =
    Segmenter.create
      ~on_open:(fun ~start:_ -> ((), Strings.empty))
      ~on_close:(fun _ -> incr closed)
      (config ~duration:1000)
  in
  ignore (Segmenter.bound seg ~length:chunk);
  Segmenter.advance seg chunk;
  Segmenter.add_bytes seg (Strings.of_string "x");
  Segmenter.split seg ~time:200 ~forced:false;
  for _ = 1 to 40 do
    check "split due: no force"
      (not (Segmenter.bound ~split_due:true seg ~length:chunk));
    Segmenter.advance seg chunk;
    Segmenter.add_bytes seg (Strings.of_string "x")
  done;
  check "split due: no cut at an earlier split point" (!closed = 0);
  check "without it the bound cuts"
    ((not (Segmenter.bound seg ~length:chunk)) && !closed = 1)

let () =
  check "T rounds 6.006 s to 6"
    (Segmenter.target_duration { duration = 6006; ticks_per_second = tps } = 6);
  check "T at least 1"
    (Segmenter.target_duration { duration = 400; ticks_per_second = tps } = 1);
  Printf.printf "segmenter: %d checks passed\n" !checks;
  if !checks < 20 then failwith "too few checks ran"
