(* An operator's sync source follows the readiness of the children it reads:
   spec/pacing.md §3. The pacer is passive, so the clock learns about it
   through the operator alone. *)

let sync = Clock.Sync_source.generic ~name:"test_pacer"

class pacer =
  object (self)
    inherit Source.source ~name:"test_pacer" ()
    val mutable ready = true
    method set_ready value = ready <- value
    method effective_source = (self :> Source.source)
    method fallible = true
    method private can_generate_frame = ready
    method self_sync = (`Dynamic, Some sync)
    method remaining = -1
    method abort_track = ()

    method private generate_frame =
      Frame.create ~length:(Lazy.Mutexed.force Frame.size) self#content_type
  end

class reader sources =
  let self_sync = Source_sync.of_sources sources in
  object (self)
    inherit Source.operator ~name:"test_reader" sources
    method effective_source = (self :> Source.source)
    method fallible = true

    method private can_generate_frame =
      List.exists (fun s -> s#is_ready) sources

    method self_sync = self_sync ~source:self ()
    method remaining = -1
    method abort_track = ()

    method private generate_frame =
      (List.find (fun s -> s#is_ready) sources)#get_frame
  end

let () = Frame_settings.lazy_config_eval := true

let audio_t =
  Lang.frame_t (Lang.univ_t ())
    (Frame.Fields.make ~audio:(Format_type.audio ()) ())

let reports reader expected = Clock.Sync_source.same reader#sync_source expected

let () =
  let clock =
    Clock.create ~sync:`Passive
      ~owner:{ Clock.kind = "test"; id = "source_sync_test" }
      ~id:"source_sync_test" ()
  in
  let pacer = new pacer in
  let reader = new reader [(pacer :> Source.source)] in
  Typing.(reader#frame_type <: audio_t);
  let output =
    new Output.dummy
      ~clock ~autostart:true ~infallible:false ~register_telnet:false
      (Lang.source (reader :> Source.source))
  in
  output#content_type_computation_allowed;
  Clock.start ~force:true clock;
  Clock.activate_pending clock;
  Clock.tick clock;
  assert (reports reader (Some sync));
  pacer#set_ready false;
  Clock.tick clock;
  assert (reports reader None);
  pacer#set_ready true;
  Clock.tick clock;
  assert (reports reader (Some sync));
  Clock.stop clock
