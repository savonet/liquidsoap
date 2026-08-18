(* Unit test for clock retention: a clock created outside any
   [Clock.with_new_clocks] frame is rooted by the registry until it starts,
   its graph being a cycle through its own outputs. *)

let[@inline never] create_unrooted id = ignore (Clock.create ~id ())

(* The registry is weak on the paths this one is not, so a collection that
   reclaimed nothing would let the assertion below pass vacuously. *)
let[@inline never] drop_witness reclaimed =
  let witness = Bytes.create 32 in
  Gc.finalise (fun _ -> reclaimed := true) witness;
  ignore (Sys.opaque_identity witness)

let () =
  Frame_settings.lazy_config_eval := true;

  let reclaimed = ref false in
  let id = "retention_unrooted" in
  create_unrooted id;
  drop_witness reclaimed;

  (* Creation raises [Effect.Unhandled] to reach the registry, and the runtime
     roots the last exception: without dropping it here the clock survives
     whichever way it was deferred. *)
  (try raise Exit with Exit -> ());

  Gc.full_major ();
  assert !reclaimed;
  assert (List.exists (fun c -> Clock.id c = id) (Clock.clocks ()));

  Printf.printf "clock_retention_test passed!\n%!"
