(* An operator's sync source follows the readiness of the children it reads,
   and the output above it tells its subscribers: spec/pacing.md §3. The pacer
   is passive, so the clock learns about it through the operator alone. *)

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

(* Plays one child, chosen while running, and only states its answer. *)
class selector children =
  object (self)
    inherit Source.operator ~name:"test_selector" children
    val mutable selected : Source.source = List.hd children
    method select source = selected <- source
    method effective_source = (self :> Source.source)
    method fallible = true
    method private can_generate_frame = selected#is_ready
    method self_sync = (`Dynamic, snd selected#self_sync)
    method remaining = -1
    method abort_track = ()
    method private generate_frame = selected#get_frame
  end

let () = Frame_settings.lazy_config_eval := true

let audio_t =
  Lang.frame_t (Lang.univ_t ())
    (Frame.Fields.make ~audio:(Format_type.audio ()) ())

let same = Clock.Sync_source.same

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
  let reports expected =
    same reader#sync_source expected && same output#sync_source expected
  in
  Clock.start ~force:true clock;
  Clock.activate_pending clock;
  Clock.tick clock;
  assert (reports (Some sync));
  pacer#set_ready false;
  Clock.tick clock;
  assert (reports None);
  pacer#set_ready true;
  Clock.tick clock;
  assert (reports (Some sync));
  Clock.stop clock

let tracked clock =
  Option.map
    (fun (sync : Clock.Status.sync) -> sync.sync_source)
    (Clock.status clock).sync

(* K6: the clock's pacing follows the child a selecting operator plays. *)
let () =
  let clock =
    Clock.create ~sync:`Passive
      ~owner:{ Clock.kind = "test"; id = "selector_test" }
      ~id:"selector_test" ()
  in
  let pacer = (new pacer :> Source.source) in
  let silent =
    (object
       inherit pacer
       method! self_sync = (`Static, None)
     end
      :> Source.source)
  in
  let selector = new selector [silent; pacer] in
  Typing.(selector#frame_type <: audio_t);
  let output =
    new Output.dummy
      ~clock ~autostart:true ~infallible:false ~register_telnet:false
      (Lang.source (selector :> Source.source))
  in
  output#content_type_computation_allowed;
  Clock.start ~force:true clock;
  Clock.activate_pending clock;
  Clock.tick clock;
  Clock.tick clock;
  assert (tracked clock = None);
  selector#select pacer;
  Clock.tick clock;
  Clock.tick clock;
  assert (tracked clock = Some "test_pacer");
  selector#select silent;
  Clock.tick clock;
  Clock.tick clock;
  assert (tracked clock = None);
  Clock.stop clock

(* The clock's pacing follows a sequence to the child it moved on to. *)
let () =
  let clock =
    Clock.create ~sync:`Passive
      ~owner:{ Clock.kind = "test"; id = "sequence_test" }
      ~id:"sequence_test" ()
  in
  let pacer = new pacer in
  let silent =
    (object
       inherit pacer
       method! self_sync = (`Static, None)
     end
      :> Source.source)
  in
  let sequence = new Sequence.sequence [(pacer :> Source.source); silent] in
  Typing.(sequence#frame_type <: audio_t);
  let output =
    new Output.dummy
      ~clock ~autostart:true ~infallible:false ~register_telnet:false
      (Lang.source (sequence :> Source.source))
  in
  output#content_type_computation_allowed;
  Clock.start ~force:true clock;
  Clock.activate_pending clock;
  Clock.tick clock;
  Clock.tick clock;
  assert (tracked clock = Some "test_pacer");
  pacer#set_ready false;
  Clock.tick clock;
  Clock.tick clock;
  Clock.tick clock;
  assert (tracked clock = None);
  Clock.stop clock

(* K7: two threads computing an operator's sync type at once both get it. *)
let () =
  let inside = Atomic.make 0 in
  let slow =
    object
      method self_sync : Source_sync.t =
        Atomic.incr inside;
        let give_up = Unix.gettimeofday () +. 2. in
        while Atomic.get inside < 2 && Unix.gettimeofday () < give_up do
          Thread.yield ()
        done;
        (`Dynamic, None)
    end
  in
  let sync_type = Source_sync.type_of_sources [slow] in
  let results = Array.make 2 `Static in
  let threads =
    List.init 2 (fun i ->
        Thread.create (fun () -> results.(i) <- sync_type ()) ())
  in
  List.iter Thread.join threads;
  assert (Atomic.get inside = 2);
  assert (results = [| `Dynamic; `Dynamic |])
