open Harness

let conflict = function Clock.Conflict _ -> true | _ -> false
let loop = function Clock.Loop _ -> true | _ -> false

let controller_conflict = function
  | Clock.Controller_conflict _ -> true
  | _ -> false

let unifies a b =
  match Clock.unify ~pos:None a b with () -> true | exception _ -> false

let basics () =
  let clock = Clock.create () in
  check "a handle unifies with itself" (unifies clock clock);
  let named = Clock.create ~id:"named" () in
  let anonymous = Clock.create () in
  Clock.unify ~pos:None named anonymous;
  check "two stopped automatic clocks unify"
    (Clock.equal named anonymous && registered named = 1);
  check "the survivor takes the other's id when it has none"
    (Clock.id anonymous = Some "named");
  let left = Clock.create ~id:"left" () in
  let right = Clock.create ~id:"right" () in
  Clock.unify ~pos:None left right;
  check "the survivor keeps its own id when both have one, and says so"
    (List.exists
       (function Clock.Event.Id_kept _ -> true | _ -> false)
       (events_of left));
  let pair mode = (Clock.create (), Clock.create ~sync:mode ()) in
  let either_order mode =
    let a, b = pair mode in
    let c, d = pair mode in
    unifies a b && unifies d c
    && Clock.sync_mode a = mode
    && Clock.sync_mode c = mode
  in
  check "an automatic clock unifies with a CPU clock, in either order"
    (either_order `Cpu);
  check "an automatic clock unifies with an unsynced clock, in either order"
    (either_order `Unsynced);
  check "two stopped CPU clocks unify"
    (unifies (Clock.create ~sync:`Cpu ()) (Clock.create ~sync:`Cpu ()));
  let cpu = Clock.create ~sync:`Cpu () in
  let unsynced = Clock.create ~sync:`Unsynced () in
  raises "a stopped CPU clock and a stopped unsynced clock conflict" conflict
    (fun () -> Clock.unify ~pos:None cpu unsynced);
  raises "the conflict does not depend on the order of the arguments" conflict
    (fun () -> Clock.unify ~pos:None unsynced cpu)

let pending_sources () =
  let a = Clock.create () and b = Clock.create () in
  let on_a = source ~id:"a" `Output and on_b = source ~id:"b" `Output in
  attach a on_a;
  attach b on_b;
  Clock.unify ~pos:None a b;
  let pending clock =
    List.sort String.compare
      (List.map (fun (s : Clock.source) -> s#id) (Clock.pending clock))
  in
  check "sources pending on either clock are pending on both handles"
    (pending a = ["a"; "b"] && pending b = ["a"; "b"])

let nesting () =
  let top = Clock.create () in
  let child = sub_clock top in
  let grandchild = sub_clock child in
  raises "unifying a clock with its sub-clock is a loop" loop (fun () ->
      Clock.unify ~pos:None top child);
  raises "unifying a clock with a clock nested deeper in it is a loop" loop
    (fun () -> Clock.unify ~pos:None grandchild top)

let owners () =
  let owned () = Clock.create ~sync:`Passive ~owner:(owner ()) () in
  raises "two passive clocks with different owners are a controller conflict"
    controller_conflict (fun () -> Clock.unify ~pos:None (owned ()) (owned ()));
  let parent = Clock.create () in
  let shared = sub_clock parent in
  raises "a passive clock with an owner and one without never unify"
    controller_conflict (fun () -> Clock.unify ~pos:None (owned ()) shared);
  let exclusive () = Clock.create ~sync:`Passive ~parent ~owner:(owner ()) () in
  let first = exclusive () and second = exclusive () in
  check "K15: two exclusive child clocks stay two"
    ((not (unifies first second)) && not (Clock.equal first second));
  let other = sub_clock parent in
  check "K15: two readers of a shared child end with one child clock"
    (unifies shared other && Clock.equal shared other)

let parents () =
  let left_parent = Clock.create () and right_parent = Clock.create () in
  let left = sub_clock left_parent and right = sub_clock right_parent in
  Clock.unify ~pos:None left right;
  check "two child clocks unify when their parents do, which are unified too"
    (Clock.equal left right && Clock.equal left_parent right_parent);
  let cpu = Clock.create ~id:"cpu" ~sync:`Cpu () in
  let unsynced = Clock.create ~id:"unsynced" ~sync:`Unsynced () in
  let under_cpu = sub_clock ~id:"under_cpu" cpu in
  let under_unsynced = sub_clock ~id:"under_unsynced" unsynced in
  attach under_cpu (source ~id:"waiting" `Output);
  attach cpu (source ~id:"waiting.above" `Output);
  let all = [cpu; unsynced; under_cpu; under_unsynced] in
  let before = List.map Clock.status all in
  raises "two child clocks whose parents conflict do not unify" conflict
    (fun () -> Clock.unify ~pos:None under_cpu under_unsynced);
  check "K11: a failed unification leaves every clock involved as it was"
    (List.map Clock.status all = before
    && (not (Clock.equal under_cpu under_unsynced))
    && List.for_all (fun clock -> registered clock <= 1) all
    && registered cpu = 1
    && registered unsynced = 1)

let merge_into_started () =
  let running = passive () in
  let child = sub_clock running in
  Clock.register ~parent:running child;
  let joining = Clock.create () in
  let output = source `Output in
  attach joining output;
  let registry_size = List.length (Clock.clocks ()) in
  Clock.unify ~pos:None joining child;
  check "K11: no set holds the clock merged away, and none a clock twice"
    (List.length (Clock.clocks ()) = registry_size - 1
    && registered running = 1
    && List.length (Clock.sub_clocks running) = 1);
  tick joining 2;
  check "K11: a merge into a started clock leaves it ticking"
    (Clock.ticks joining = Some 2 && Clock.started joining);
  check "K11: the sources of the other clock join at its next tick"
    (output#animated = 2 && output#awake = 1)

let sub_clocks_of_one_parent () =
  let parent = passive () in
  let first = sub_clock parent and second = sub_clock parent in
  Clock.register ~parent first;
  Clock.register ~parent second;
  Clock.stop first;
  Clock.stop second;
  Clock.unify ~pos:None first second;
  check "K3: two unified sub-clocks of one parent are one entry"
    (List.length (Clock.sub_clocks parent) = 1);
  Clock.deregister ~parent first;
  check "K3: the entry keeps the registrants of both"
    (List.length (Clock.sub_clocks parent) = 1);
  Clock.start ~force:true first;
  let inner = source `Output in
  attach second inner;
  tick parent 3;
  check "K3: it is prepared by the parent's tick" (inner#awake = 1)

let ancestors clock =
  let rec climb seen clock =
    match Clock.parent clock with
      | None -> true
      | Some parent ->
          (not (List.exists (Clock.equal parent) seen))
          && climb (parent :: seen) parent
  in
  climb [clock] clock

let no_cycle () =
  Random.init 42;
  let clocks = ref [||] in
  let pick () = !clocks.(Random.int (Array.length !clocks)) in
  clocks := Array.init 8 (fun _ -> Clock.create ());
  for _ = 1 to 2000 do
    (match Random.int 3 with
      | 0 -> clocks := Array.append !clocks [| sub_clock (pick ()) |]
      | 1 -> ( try Clock.unify ~pos:None (pick ()) (pick ()) with _ -> ())
      | _ -> (
          let sub = pick () in
          try Clock.register ~parent:(pick ()) sub with _ -> ()));
    if Array.length !clocks > 64 then clocks := Array.sub !clocks 32 32
  done;
  check "K18: no creation, registration or unification nests a clock in itself"
    (Array.for_all ancestors !clocks)

let deduplication () =
  let clocks =
    List.init 20 (fun i ->
        Clock.create ~on_error:(fun _ _ -> ignore (Sys.opaque_identity i)) ())
  in
  check "K4: deduplicating distinct clocks never fails"
    (match List.sort_uniq Clock.compare (clocks @ clocks) with
      | distinct -> List.length distinct = 20
      | exception _ -> false)

(* Merging a shared clock into a fresh one moves the shared root every time,
   so that unifications really race on it. *)
let concurrent () =
  let handles = Array.init 8 (fun _ -> Clock.create ()) in
  let failures = Atomic.make 0 in
  let worker seed () =
    let state = Random.State.make [| seed |] in
    let pick () = handles.(Random.State.int state 8) in
    for _ = 1 to 3000 do
      try
        let fresh = Clock.create () and shared = pick () in
        attach fresh (source `Passive);
        if Random.State.bool state then Clock.unify ~pos:None shared fresh
        else Clock.unify ~pos:None fresh shared;
        ignore (Clock.name (pick ()), Clock.status (pick ()));
        if not (Clock.equal fresh shared) then Atomic.incr failures
      with error ->
        if Atomic.fetch_and_add failures 1 = 0 then
          prerr_endline (Printexc.to_string error)
    done
  in
  let domains = List.init 4 (fun seed -> Domain.spawn (worker seed)) in
  List.iter Domain.join domains;
  check "K11: threads unifying and reading at once never fail"
    (Atomic.get failures = 0);
  Array.iter (Clock.unify ~pos:None handles.(0)) handles;
  check "K11: every handle still designates a clock, held once"
    (Array.for_all (fun handle -> registered handle = 1) handles
    && Array.for_all (Clock.equal handles.(0)) handles);
  check "K11: no pending source was lost by unifications racing"
    (List.length (Clock.pending handles.(0)) = 4 * 3000)

(* Finalised clocks leave the registry at its next use, one cycle later. *)
(* Sources attached through a handle while its clock is merged away must all
   end up pending on the survivor. *)
let attach_during_merges () =
  let handle = Clock.create () in
  let count = 3000 in
  let attaching =
    Domain.spawn (fun () ->
        for _ = 1 to count do
          attach handle (source `Passive)
        done)
  in
  let merging =
    Domain.spawn (fun () ->
        for _ = 1 to count do
          Clock.unify ~pos:None handle (Clock.create ())
        done)
  in
  Domain.join attaching;
  Domain.join merging;
  check
    (Printf.sprintf "K11: sources attached during merges are all kept: %d of %d"
       (List.length (Clock.pending handle))
       count)
    (List.length (Clock.pending handle) = count)

let live_words () =
  clear_events ();
  for _ = 1 to 3 do
    collect ();
    ignore (Clock.clocks ())
  done;
  (Gc.stat ()).live_words

let churn main count =
  for _ = 1 to count do
    let track = Sys.opaque_identity (Clock.create ()) in
    let child = sub_clock track in
    attach track (source `Passive);
    Clock.unify ~pos:None track main;
    ignore (Sys.opaque_identity child);
    Clock.tick main
  done

(* The leak warning is off: its report, kept by the log, would be what grows. *)
let flat_memory () =
  Clock.Settings.conf_leak_warning#set 0;
  let main = passive () in
  churn main 20000;
  let before = live_words () in
  churn main 20000;
  let after = live_words () in
  check
    (Printf.sprintf
       "K11: creating, unifying and discarding clocks uses flat memory (%d -> \
        %d words)"
       before after)
    (after < before + 10000)

let run () =
  basics ();
  pending_sources ();
  nesting ();
  owners ();
  parents ();
  merge_into_started ();
  sub_clocks_of_one_parent ();
  no_cycle ();
  deduplication ();
  concurrent ();
  attach_during_merges ();
  flat_memory ()
