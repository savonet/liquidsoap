(* Unit test for clock retention: a clock created outside any
   [Clock.with_new_clocks] frame is rooted by the registry until it starts,
   its graph being a cycle through its own outputs. *)

let[@inline never] create_unrooted id = ignore (Clock.create ~id ())

(* The registry is weak on the paths this one is not, so a collection that
   reclaimed nothing would let the assertion below pass vacuously. A weak
   pointer rather than a finaliser: clearing is part of the major cycle,
   where a finaliser only runs at the next poll point. *)
let[@inline never] weak_witness () =
  let witness = Weak.create 1 in
  Weak.set witness 0 (Some (Bytes.create 32));
  witness

let () =
  Frame_settings.lazy_config_eval := true;

  let id = "retention_unrooted" in
  create_unrooted id;
  let witness = weak_witness () in

  (* Creation raises [Effect.Unhandled] to reach the registry, and the runtime
     roots the last exception: without dropping it here the clock survives
     whichever way it was deferred. *)
  (try raise Exit with Exit -> ());

  Gc.full_major ();
  Gc.full_major ();
  assert (not (Weak.check witness 0));
  assert (List.exists (fun c -> Clock.id c = id) (Clock.clocks ()));

  Printf.printf "clock_retention_test passed!\n%!"
