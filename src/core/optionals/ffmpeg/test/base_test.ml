(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Liquidsoap_ffmpeg

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let tb num den = { Avutil.num; den }

(* common.md §5 conformance. *)
let rescale () =
  let r t src dst = Time_base.rescale ~src ~dst t in
  check "23 ms at 44100" (r 23 (tb 1 1000) (tb 1 44100) = Some 1014);
  check "-1 rounds to 0" (r (-1) (tb 1 90000) (tb 1 44100) = Some 0);
  check "-2 rounds to -1" (r (-2) (tb 1 90000) (tb 1 44100) = Some (-1));
  check "overflow" (r (1 lsl 61) (tb 1 1) (tb 1 90000) = None);
  let t = 1 lsl 40 in
  check "exact large"
    (r t (tb 1 90000) (tb 1 44100)
    = Some (int_of_float (Float.round (float t *. 44100. /. 90000.))));
  check "symmetric" (r (-1014) (tb 1 44100) (tb 1 1000) = Some (-23))

type item = { mutable ts : Int64.t option; duration : Int64.t option }

let accessors =
  {
    Duration.get_ts = (fun i -> i.ts);
    set_ts = (fun i ts -> i.ts <- ts);
    get_duration = (fun i -> i.duration);
  }

let faults () = Faults.create (Log.make ["test"])

let helper ?(convert_ts = false) src =
  Duration.create ~convert_ts ~src ~faults:(faults ()) accessors

(* Lengths of the pushed chunks, then with the flushed one: the flush measures
   from an absolute end, so only the first shows accumulated drift. *)
let totals helper items =
  let length = ref 0 in
  List.iter
    (fun i ->
      match Duration.push helper i with
        | `Chunk { Duration.length = l } -> length := !length + l
        | _ -> ())
    items;
  let pushed = !length in
  match Duration.flush helper with
    | Some { Duration.length = l } -> (pushed, pushed + l)
    | None -> (pushed, pushed)

(* common.md §6.6: ten hours of 23/24 ms steps in 1/1000. *)
let long_stream () =
  let n = 10 * 3600 * 1000 / 23 in
  let ts = ref 0 in
  let items =
    List.init n (fun i ->
        let step = if i mod 3 = 0 then 24 else 23 in
        let item =
          { ts = Some (Int64.of_int !ts); duration = Some (Int64.of_int step) }
        in
        ts := !ts + step;
        item)
  in
  let last_end = !ts in
  let last_ts = last_end - if (n - 1) mod 3 = 0 then 24 else 23 in
  let pushed, length = totals (helper (tb 1 1000)) items in
  let to_main t =
    Option.get (Time_base.rescale ~src:(tb 1 1000) ~dst:(tb 1 44100) t)
  in
  check "ten hours of chunks within one tick"
    (abs (pushed - to_main last_ts) <= 1);
  check "ten hours within one tick" (abs (length - to_main last_end) <= 1);
  let video_step = 33 in
  let video =
    List.init (last_end / video_step) (fun i ->
        { ts = Some (Int64.of_int (i * video_step)); duration = Some 33L })
  in
  let _, video_length = totals (helper (tb 1 1000)) video in
  let video_span = last_end / video_step * video_step in
  let span_diff =
    Option.get (Time_base.rescale ~src:(tb 1 1000) ~dst:(tb 1 44100) last_end)
    - Option.get
        (Time_base.rescale ~src:(tb 1 1000) ~dst:(tb 1 44100) video_span)
  in
  check "two streams within two ticks"
    (abs (length - video_length - span_diff) <= 2)

let inline_aac () =
  let n = 10000 in
  let items =
    List.init n (fun i ->
        { ts = Some (Int64.of_int (i * 1024)); duration = None })
  in
  let h = helper (tb 1 48000) in
  let length = ref 0 in
  List.iter
    (fun i ->
      match Duration.push h i with
        | `Chunk { Duration.length = l } -> length := !length + l
        | _ -> ())
    items;
  (* n - 1 chunks emitted, the last group still held. *)
  check "inline aac" (abs (!length - int_of_float (float (n - 1) *. 940.8)) <= 1)

let edge_cases () =
  let h = helper (tb 1 1000) in
  let a = { ts = Some 10L; duration = None } in
  let b = { ts = Some 10L; duration = None } in
  let c = { ts = Some 20L; duration = None } in
  ignore (Duration.push h a);
  ignore (Duration.push h b);
  (match Duration.push h c with
    | `Chunk { Duration.items = [x; y] } ->
        check "equal ts one chunk" (x == a && y == b)
    | _ -> check "equal ts one chunk" false);
  check "decreasing dropped"
    (Duration.push h { ts = Some 5L; duration = None } = `Dropped);
  check "state unchanged" (Duration.last_ts h = Some 20);
  check "next increasing accepted"
    (Duration.push h { ts = Some 30L; duration = None } <> `Dropped);
  let h = helper (tb 1 1000) in
  ignore (Duration.push h { ts = Some 100L; duration = None });
  let result = ref `Nothing in
  for _ = 1 to Faults.max_consecutive_faults do
    result := Duration.push h { ts = Some 1L; duration = None }
  done;
  check "faults give discontinuity"
    (match !result with `Discontinuity _ -> true | _ -> false);
  let h = helper (tb 1 1000) in
  check "flush empty" (Duration.flush h = None);
  check "push after flush raises"
    (try
       ignore (Duration.push h { ts = Some 1L; duration = None });
       false
     with Failure _ -> true)

module Int_runs =
  Runs.Make
    (struct
      type t = unit

      let known _ = true
    end)
    (struct
      type t = int

      let copy = Fun.id
      let checksum = string_of_int
    end)

let positions (d : Int_runs.data) =
  List.map
    (fun (start, (r : Int_runs.run)) ->
      ( r.Int_runs.source_id,
        start,
        r.length,
        List.map (fun (pos, _) -> start + pos) r.items ))
    d.runs

let one_run ~id ~length items =
  Int_runs.of_run ()
    (Int_runs.run ~source_id:id ~time_base:(tb 1 1000) ~length
       (List.map (fun p -> (p, p)) items))

(* content.md §6.6 blit table. *)
let blit () =
  let src = one_run ~id:2 ~length:100 [15; 35; 55; 75] in
  let dst = one_run ~id:1 ~length:100 [5; 25; 50; 85] in
  Int_runs.blit src 20 dst 20 40;
  check "blit row 1"
    (positions dst = [(1, 0, 20, [5]); (2, 20, 40, [35; 55]); (1, 60, 40, [85])]
    && dst.length = 100);
  let dst = Int_runs.make () in
  Int_runs.blit (Int_runs.make ~length:30 ()) 0 dst 0 30;
  Int_runs.blit (one_run ~id:3 ~length:50 [0; 20]) 0 dst 30 50;
  check "blit row 2" (positions dst = [(3, 30, 50, [30; 50])] && dst.length = 80);
  let dst = Int_runs.make () in
  Int_runs.blit (one_run ~id:3 ~length:40 []) 0 dst 0 40;
  Int_runs.blit (one_run ~id:4 ~length:40 []) 0 dst 40 40;
  check "blit row 3" (List.length dst.runs = 2 && dst.length = 80);
  let dst = Int_runs.make () in
  Int_runs.blit (one_run ~id:3 ~length:40 [0]) 0 dst 0 40;
  Int_runs.blit (one_run ~id:3 ~length:40 [10]) 0 dst 40 40;
  check "blit row 4" (positions dst = [(3, 0, 80, [0; 50])]);
  let a = one_run ~id:1 ~length:100 [] and b = one_run ~id:1 ~length:100 [] in
  b.runs <- [(0, { (snd (List.hd b.runs)) with length = 90 })];
  check "checksum sees run length" (Int_runs.checksum a <> Int_runs.checksum b)

let () =
  Frame_settings.lazy_config_eval := true;
  rescale ();
  long_stream ();
  inline_aac ();
  edge_cases ();
  blit ();
  let expected = 22 in
  if !checks <> expected then
    failwith (Printf.sprintf "ran %d checks, expected %d" !checks expected);
  Printf.printf "base_test: %d checks passed\n" !checks
