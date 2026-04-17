(* Regression test: sub-clocks deregistered by on_sleep must still be
   stopped when the parent clock stops.

   The bug: has_stopped first sleeps outputs (which fires on_sleep callbacks
   that deregister sub-clocks), then iterates sub_clocks to stop them — but
   by then the sub-clock is already gone and never gets stopped.

   The fix: snapshot sub_clocks before sleeping outputs. *)

class test_output ~clock source =
  object
    inherit
      Output.dummy
        ~clock ~autostart:true ~infallible:false ~register_telnet:false
        (Lang.source (source :> Source.source))

    method! can_generate_frame = false
  end

class test_source =
  object (self)
    inherit Debug_sources.fail "test_source"
    method! can_generate_frame = false
    method! generate_frame = self#empty_frame
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let parent_clock = Clock.create ~sync:`Passive () in
  let sub_clock = Clock.create ~sync:`Passive () in
  Clock.register_sub_clock parent_clock sub_clock;
  let source = new test_source in
  let output = new test_output ~clock:parent_clock source in
  (* Simulate what ffmpeg filter on_sleep does: deregister the sub-clock
     when the output source sleeps. *)
  output#on_sleep (fun () -> Clock.deregister_sub_clock parent_clock sub_clock);
  output#content_type_computation_allowed;
  Clock.start ~force:true parent_clock;
  Clock.start ~force:true sub_clock;
  (* Activate pending sources so the output is in clock.outputs,
     without generating frames. *)
  Clock.activate_pending_sources parent_clock;
  (* Stopping the parent clock must stop the sub_clock even though
     on_sleep will deregister it from sub_clocks mid-stop. *)
  Clock.stop parent_clock;
  assert (not (Clock.started sub_clock));
  Printf.printf "sub_clock_stop_test passed!\n%!"
