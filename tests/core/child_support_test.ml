(* Unit tests for Child_support producers:
   - a producer ticks its child clock only when it needs data,
   - two producers sharing a child clock share ticks: data produced during a
     tick issued by one is buffered for the other, which reads it without
     ticking the clock again,
   - a custom process_frame receives `Flush when the producer sleeps,
   - two producers sharing a child clock but consuming at diverging rates
     raise instead of buffering without bound. *)

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
      Frame.create ~length:(Lazy.Mutexed.force Frame.size) self#content_type
  end

class test_output ~clock source =
  object
    inherit
      Output.dummy
        ~clock ~autostart:true ~infallible:false ~register_telnet:false
        (Lang.source (source :> Source.source))
  end

let () = Frame_settings.lazy_config_eval := true

let audio_t =
  Lang.frame_t (Lang.univ_t ())
    (Frame.Fields.make ~audio:(Format_type.audio ()) ())

let producer ~name source =
  new Child_support.producer
    ~check_self_sync:true ~name
    (Lang.source (source :> Source.source))

let () =
  let parent = Clock.create ~sync:`Passive ~id:"child_support_test" () in
  let child_source = new ready_source in
  let producer_1 = producer ~name:"producer_1" child_source in
  let producer_2 = producer ~name:"producer_2" child_source in
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

  (* Animating the child clock, as its parent does to keep the outputs and
     active sources it may contain running, must not buffer any data. *)
  Clock.tick producer_1#child_clock;
  assert (Generator.length producer_1#child_buffer = 0);
  assert (Generator.length producer_2#child_buffer = 0);

  (* Stopping the parent clock sleeps the producers, which flush their child
     output. *)
  assert (not !flushed);
  Clock.stop parent;
  assert !flushed

let () =
  Child_support.conf_max_buffer#set 0.5;
  let parent = Clock.create ~sync:`Passive ~id:"child_support_diverging" () in
  let child_source = new ready_source in
  let fast = producer ~name:"fast" child_source in
  let slow = producer ~name:"slow" child_source in
  Typing.(fast#frame_type <: audio_t);
  Typing.(slow#frame_type <: audio_t);
  let output_fast = new test_output ~clock:parent fast in
  let output_slow = new test_output ~clock:parent slow in
  output_fast#content_type_computation_allowed;
  output_slow#content_type_computation_allowed;

  (* [slow] keeps half of each child frame, so it needs two child clock ticks
     per parent tick where [fast] needs one: [fast] is fed data it never
     catches up on. *)
  slow#child#set_process_frame (fun generator -> function
    | `Frame frame ->
        Generator.append ~length:(Frame.position frame / 2) generator frame
    | `Flush -> ());

  Clock.start ~force:true parent;
  Clock.activate_pending_sources parent;

  let raised = ref false in
  (try
     for _ = 1 to 100 do
       Clock.tick parent
     done
   with Runtime_error.Runtime_error { kind = "clock" } -> raised := true);
  assert !raised;

  Printf.printf "child_support_test passed!\n%!"
