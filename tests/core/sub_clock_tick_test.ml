(* Unit tests for sub-clock ticking:
   - sub-clocks are ticked as part of their parent's tick, recursively,
   - a sub-clock already ticked during the parent's tick (e.g. by an
     operator pulling data from it) is not ticked again,
   - deregistered sub-clocks are no longer ticked,
   - after unification, a parent's duplicate sub-clock handles are
     deduplicated, including for passive parents. *)

let () =
  Frame_settings.lazy_config_eval := true;

  let parent = Clock.create ~sync:`Passive ~id:"tick_parent" () in
  let sub = Clock.create ~sync:`Passive ~id:"tick_sub" () in
  let sub_sub = Clock.create ~sync:`Passive ~id:"tick_sub_sub" () in
  Clock.register_sub_clock parent sub;
  Clock.register_sub_clock sub sub_sub;
  Clock.start ~force:true parent;
  Clock.start ~force:true sub;
  Clock.start ~force:true sub_sub;

  (* Ticking the parent ticks the whole sub-clock hierarchy. *)
  Clock.tick parent;
  assert (Clock.ticks parent = 1);
  assert (Clock.ticks sub = 1);
  assert (Clock.ticks sub_sub = 1);

  (* A sub-clock ticked during the parent's tick is not ticked again: tick
     it from an on_tick callback, as an operator pulling data would. *)
  Clock.on_tick parent (fun () -> Clock.tick sub);
  Clock.tick parent;
  assert (Clock.ticks parent = 2);
  assert (Clock.ticks sub = 2);
  assert (Clock.ticks sub_sub = 2);

  (* Deregistered sub-clocks are no longer ticked. *)
  Clock.deregister_sub_clock parent sub;
  Clock.tick parent;
  assert (Clock.ticks parent = 3);
  assert (Clock.ticks sub = 2);
  assert (Clock.ticks sub_sub = 2);
  Clock.register_sub_clock parent sub;

  (* Register a second sub-clock and unify it with the first: both handles
     now dereference to a single clock and the parent's sub_clocks list is
     deduplicated accordingly, even though the parent is passive. *)
  let sub' = Clock.create ~sync:`Passive ~id:"tick_sub_alias" () in
  Clock.register_sub_clock parent sub';
  Clock.unify ~pos:None sub' sub;
  assert (List.length (Clock.sub_clocks parent) = 1);
  Clock.tick parent;
  assert (Clock.ticks parent = 4);
  assert (Clock.ticks sub = 3);
  assert (Clock.ticks sub' = 3);
  assert (Clock.ticks sub_sub = 3);

  Printf.printf "sub_clock_tick_test passed!\n%!"
