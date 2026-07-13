(* Unit tests for Child_support producers:
   - a producer ticks its child clock only when it needs data,
   - two producers sharing a child clock share ticks: data produced during a
     tick issued by one is buffered for the other, which reads it without
     ticking the clock again,
   - a custom process_frame receives `Flush when the producer sleeps. *)

class ready_source =
  object (self)
    inherit Source.source ~name:"test_ready" ()
    method effective_source = (self :> Source.source)
    method fallible = false
    method private can_generate_frame = true
    method self_sync = (`Static, None)
    method remaining = -1
    method abort_track = ()

    method private generate_frame =
      Frame.create ~length:(Lazy.force Frame.size) self#content_type
  end

class test_output ~clock source =
  object
    inherit
      Output.dummy
        ~clock ~autostart:true ~infallible:false ~register_telnet:false
        (Lang.source (source :> Source.source))
  end

let () =
  Frame_settings.lazy_config_eval := true;
  let audio_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  let parent = Clock.create ~sync:`Passive ~id:"child_support_test" () in
  let child_source = new ready_source in
  let producer_1 =
    new Child_support.producer
      ~check_self_sync:true ~name:"producer_1"
      (Lang.source (child_source :> Source.source))
  in
  let producer_2 =
    new Child_support.producer
      ~check_self_sync:true ~name:"producer_2"
      (Lang.source (child_source :> Source.source))
  in
  Typing.(producer_1#frame_type <: audio_t);
  Typing.(producer_2#frame_type <: audio_t);
  let output_1 = new test_output ~clock:parent producer_1 in
  let output_2 = new test_output ~clock:parent producer_2 in
  output_1#content_type_computation_allowed;
  output_2#content_type_computation_allowed;
  let flushed = ref false in
  producer_2#child#set_process_frame (fun generator -> function
    | `Frame frame -> Generator.append generator frame
    | `Flush -> flushed := true);

  (* Both producers wrap the same source: their child clocks are unified. *)
  assert (
    Clock.ticks producer_1#child_clock = Clock.ticks producer_2#child_clock);

  Clock.start ~force:true parent;
  Clock.activate_pending_sources parent;

  (* First parent tick: producer_1 ticks the child clock to fill its buffer;
     the same tick also fills producer_2's buffer, so producer_2 reads
     without ticking again. *)
  Clock.tick parent;
  assert (Clock.ticks producer_1#child_clock = 1);

  (* Same balance on subsequent ticks: one child tick per parent tick. *)
  Clock.tick parent;
  assert (Clock.ticks producer_1#child_clock = 2);

  (* Both buffers were fully consumed each cycle. *)
  assert (Generator.length producer_1#child_buffer = 0);
  assert (Generator.length producer_2#child_buffer = 0);

  (* Stopping the parent clock sleeps the producers, which flush their child
     output. *)
  assert (not !flushed);
  Clock.stop parent;
  assert !flushed;

  Printf.printf "child_support_test passed!\n%!"
