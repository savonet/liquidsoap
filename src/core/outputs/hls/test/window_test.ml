(* Conformance checks of spec/playlists.md §2, §2.1 and spec/files.md §5. *)

open Hls_output

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let publish ?(discontinuous = false) w duration =
  Window.publish w ~discontinuous ~forced:false ~codecs:None ~init:None
    ~name:"s" ~tags:[] ~duration ~size:100

(* Runs the caller's order: publish, slide, list, expire. Returns what was
   deleted at each step. *)
let step w ~max_segments ~target ~overhead ?discontinuous duration =
  ignore (publish ?discontinuous w duration);
  Window.slide w ~max_segments ~target;
  Window.mark_listed w;
  Window.expire w ~overhead

(* KC7: segments=3, overhead=1, T=1: a segment stays at least its length plus
   the window after leaving it, and is gone once neither allowance holds. *)
let () =
  let w = Window.create () in
  let deleted = ref [] in
  let left = Hashtbl.create 10 in
  for i = 1 to 40 do
    let d = step w ~max_segments:3 ~target:1 ~overhead:(Some 1) 1. in
    List.iter (fun s -> deleted := (s.Window.position, i) :: !deleted) d;
    List.iter
      (fun s ->
        if
          s.Window.position < w.window_start
          && not (Hashtbl.mem left s.Window.position)
        then Hashtbl.add left s.Window.position i)
      w.segments;
    check "media sequence = segments that left"
      (Window.media_sequence w = w.window_start - 1);
    check "window at least 3 T"
      (Window.duration (Window.listed w) >= 3. || i < 3)
  done;
  check "segments deleted" (List.length !deleted > 30);
  List.iter
    (fun (position, at) ->
      match Hashtbl.find_opt left position with
        | Some left_at ->
            check "availability: length plus window after leaving"
              (at - left_at >= 1 + 3)
        | None -> check "deleted only after leaving" false)
    !deleted;
  check "bounded retention" (List.length w.segments <= 3 + 5)

(* Window: short segments after long ones keep 3 T, and the start never
   moves back. *)
let () =
  let w = Window.create () in
  let last_start = ref 0 in
  List.iter
    (fun d ->
      ignore (step w ~max_segments:3 ~target:6 ~overhead:None d);
      check "start never back" (w.window_start >= !last_start);
      last_start := w.window_start;
      check "3 T once reachable"
        (Window.duration (Window.listed w) >= 18.
        || Window.duration w.segments < 18.))
    [6.; 6.; 6.; 6.; 1.; 1.; 1.; 1.; 1.; 6.; 6.];
  check "overhead null keeps everything" (List.length w.segments = 11)

(* KC8: the discontinuity sequence counts discontinuities that left. *)
let () =
  let w = Window.create () in
  List.iteri
    (fun i _ ->
      ignore
        (step w ~max_segments:3 ~target:1 ~overhead:None ~discontinuous:(i = 2)
           1.))
    (List.init 10 Fun.id);
  check "discontinuity sequence after scroll"
    (Window.discontinuity_sequence w = 1);
  check "first listed segment's value"
    ((List.hd (Window.listed w)).discontinuity = 1)

(* Restore: counters continue, retention only longer. *)
let () =
  let saved =
    List.map
      (fun position ->
        ( {
            Persist.position;
            discontinuous = false;
            discontinuity = 0;
            forced = false;
            codecs = None;
            init = None;
            name = "s";
            tags = [];
            duration = 1.;
          },
          100 ))
      [4; 5; 6; 7; 8]
  in
  let w =
    Window.restore ~next_position:9 ~next_init_position:0 ~discontinuity_count:0
      ~window_start:6 saved
  in
  check "restored media sequence" (Window.media_sequence w = 5);
  check "next position" ((publish w 1.).position = 9);
  check "restored segments kept at the first close"
    (Window.expire w ~overhead:(Some 0) = [])

let () =
  Printf.printf "window: %d checks passed\n" !checks;
  if !checks < 50 then failwith "too few checks ran"
