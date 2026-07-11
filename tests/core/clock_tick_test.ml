(* Unit tests for clock ticking and lifecycle:
   - state and tick reporting across start/tick/stop,
   - stream time progression,
   - one-shot on_tick/after_tick callbacks and their ordering. *)

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
  let clock = Clock.create ~sync:`Passive ~id:"tick_test" () in
  assert (Clock.sync clock = `Stopped);
  assert (not (Clock.started clock));
  assert (Clock.ticks clock = 0);
  assert (Clock.time clock = -1.);

  let source = new test_source in
  let output = new test_output ~clock source in
  output#content_type_computation_allowed;
  Clock.start ~force:true clock;
  assert (Clock.started clock);
  assert (Clock.sync clock = `Passive);
  assert (Clock.ticks clock = 0);
  Clock.activate_pending_sources clock;
  assert (List.length (Clock.outputs clock) = 1);

  (* on_tick runs during the tick, after_tick after it; both are one-shot. *)
  let order = ref [] in
  Clock.on_tick clock (fun () -> order := "on_tick" :: !order);
  Clock.after_tick clock (fun () -> order := "after_tick" :: !order);
  Clock.tick clock;
  assert (Clock.ticks clock = 1);
  assert (!order = ["after_tick"; "on_tick"]);
  Clock.tick clock;
  assert (Clock.ticks clock = 2);
  assert (List.length !order = 2);

  (* Stream time is ticks * frame duration. *)
  assert (Clock.time clock = 2. *. Lazy.force Frame.duration);

  (* Stopping a passive clock is immediate. *)
  Clock.stop clock;
  assert (not (Clock.started clock));
  assert (Clock.sync clock = `Stopped);
  assert (Clock.ticks clock = 0);

  Printf.printf "clock_tick_test passed!\n%!"
