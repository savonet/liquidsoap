(* Unit tests for clock unification:
   - sync mode compatibility rules,
   - id and pending source inheritance,
   - sub-clock loop detection and deduplication,
   - controller compatibility. *)

class test_source =
  object (self)
    inherit Debug_sources.fail "test_source"
    method! can_generate_frame = false
    method! generate_frame = self#empty_frame
  end

class test_output ~clock source =
  object
    inherit
      Output.dummy
        ~clock ~autostart:true ~infallible:false ~register_telnet:false
        (Lang.source (source :> Source.source))

    method! can_generate_frame = false
  end

let () =
  Frame_settings.lazy_config_eval := true;

  (* Unifying a handle with itself is a no-op. *)
  let c = Clock.create () in
  Clock.unify ~pos:None c c;

  (* Two stopped automatic clocks unify; the right-hand clock's record wins
     and inherits the left-hand clock's id when it has none. *)
  let named = Clock.create ~id:"unify_left" () in
  let unnamed = Clock.create () in
  Clock.unify ~pos:None named unnamed;
  assert (Clock.id named = Clock.id unnamed);
  assert (Clock.id unnamed = "unify_left");

  (* When both clocks are named, the right-hand clock keeps its id. *)
  let left = Clock.create ~id:"unify_left_named" () in
  let right = Clock.create ~id:"unify_right_named" () in
  let right_id = Clock.id right in
  Clock.unify ~pos:None left right;
  assert (Clock.id left = right_id);

  (* An automatic clock unifies with a clock of any sync mode. *)
  Clock.unify ~pos:None (Clock.create ()) (Clock.create ~sync:`CPU ());
  Clock.unify ~pos:None (Clock.create ~sync:`Unsynced ()) (Clock.create ());

  (* Stopped clocks with the same non-automatic sync mode unify. *)
  Clock.unify ~pos:None
    (Clock.create ~sync:`CPU ())
    (Clock.create ~sync:`CPU ());

  (* Incompatible sync modes conflict. *)
  (try
     Clock.unify ~pos:None
       (Clock.create ~sync:`CPU ())
       (Clock.create ~sync:`Unsynced ());
     assert false
   with Liquidsoap_lang.Error.Clock_conflict _ -> ());

  (* Pending sources are inherited: sources attached to either handle before
     unification are visible through both afterwards. The output's child
     source attaches to its own clock, which is unified in when the output is
     created, so both end up pending here. *)
  let c = Clock.create ~id:"unify_attach" () in
  let c' = Clock.create () in
  let source = new test_source in
  let output = new test_output ~clock:c source in
  output#content_type_computation_allowed;
  assert (List.length (Clock.pending_activations c) = 2);
  Clock.unify ~pos:None c c';
  assert (List.length (Clock.pending_activations c) = 2);
  assert (List.length (Clock.pending_activations c') = 2);

  (* Unifying a clock with one of its (transitive) sub-clocks is a loop. *)
  let parent = Clock.create ~id:"unify_loop_parent" () in
  let sub = Clock.create ~id:"unify_loop_sub" () in
  Clock.register_sub_clock parent sub;
  (try
     Clock.unify ~pos:None parent sub;
     assert false
   with Liquidsoap_lang.Error.Clock_loop _ -> ());

  (* After unification, sub-clock handles dereferencing to the same clock
     are deduplicated. *)
  let parent = Clock.create ~id:"unify_dedup_parent" () in
  let sub = Clock.create ~id:"unify_dedup_sub" () in
  let sub' = Clock.create () in
  Clock.register_sub_clock parent sub;
  Clock.register_sub_clock parent sub';
  assert (List.length (Clock.sub_clocks parent) = 2);
  Clock.unify ~pos:None sub sub';
  assert (List.length (Clock.sub_clocks parent) = 1);

  (* Clocks with distinct external controllers cannot be unified. *)
  let controller name =
    `Other
      ( "test",
        object
          method id = name
        end )
  in
  (try
     Clock.unify ~pos:None
       (Clock.create ~controller:(controller "first") ())
       (Clock.create ~controller:(controller "second") ());
     assert false
   with Liquidsoap_lang.Error.Clock_main _ -> ());

  (* A clock without a controller unifies with a controlled one. *)
  Clock.unify ~pos:None (Clock.create ())
    (Clock.create ~controller:(controller "third") ());

  Printf.printf "clock_unify_test passed!\n%!"
