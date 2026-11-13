(* Conformance checks of spec/persistence.md §2 and §4 step 1. *)

open Hls_output

let checks = ref 0

let check name cond =
  incr checks;
  if not cond then failwith ("check failed: " ^ name)

let segment position =
  {
    Persist.position;
    discontinuous = position = 3;
    discontinuity = (if position >= 3 then 1 else 0);
    forced = false;
    codecs = (if position = 2 then None else Some "mp4a.40.2");
    init = (if position = 2 then Some "a_0.mp4" else None);
    name = Printf.sprintf "a_%d.ts" position;
    tags = (if position = 2 then ["#EXT-X-CUE"] else []);
    duration = 6.006_001;
  }

let state =
  [
    {
      Persist.name = "a";
      next_position = 5;
      next_init_position = 1;
      discontinuity_count = 1;
      window_start = 2;
      pending_tags = ["#EXT-X-PENDING"];
      segments = List.map segment [1; 2; 3; 4];
    };
  ]

let valid_name n = n <> "" && n.[0] <> '/'
let load ?(names = ["a"]) s = Persist.of_string ~names ~valid_name s
let saved = Persist.to_string state

let () =
  check "round trip" (load saved = Ok state);
  check "microsecond precision"
    (match load saved with
      | Ok [v] ->
          List.for_all
            (fun s -> Float.abs (s.Persist.duration -. 6.006_001) < 1e-7)
            v.segments
      | _ -> false);
  check "truncated"
    (Result.is_error (load (String.sub saved 0 (String.length saved / 2))));
  check "corrupt" (Result.is_error (load "{\"variants\": 3}"));
  check "other names" (Result.is_error (load ~names:["b"] saved));
  check "unknown keys ignored"
    (Result.is_ok
       (load (String.sub saved 0 (String.length saved - 1) ^ ", \"extra\": 1}")));
  let bad f = Persist.to_string [f (List.hd state)] in
  check "positions not increasing"
    (Result.is_error
       (load (bad (fun v -> { v with segments = List.rev v.segments }))));
  check "position at next_position"
    (Result.is_error (load (bad (fun v -> { v with next_position = 4 }))));
  check "discontinuity above count"
    (Result.is_error (load (bad (fun v -> { v with discontinuity_count = 0 }))));
  check "invalid name"
    (Result.is_error
       (load
          (bad (fun v ->
               { v with segments = [{ (segment 1) with name = "/etc/x" }] }))));
  check "negative count"
    (Result.is_error
       (load "{\"variants\":[{\"name\":\"a\",\"next_position\":-1}]}"));
  Printf.printf "persist: %d checks passed\n" !checks;
  if !checks < 11 then failwith "too few checks ran"
