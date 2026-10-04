(* Doubles and helpers for the checks of spec/conformance.md. *)

let checks = ref 0
let failures = ref 0

let check name passed =
  incr checks;
  if passed then Printf.printf "ok: %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL: %s\n%!" name
  end

let raises name expected fn =
  check name (match fn () with _ -> false | exception error -> expected error)

(* A run that checked nothing must not read as a pass. *)
let finish group =
  if !checks = 0 then begin
    Printf.printf "FAIL: group %s ran no check\n%!" group;
    exit 1
  end;
  Printf.printf "%s: %d checks, %d failures\n%!" group !checks !failures;
  exit (if !failures = 0 then 0 else 1)

let frame_duration =
  Frame_settings.lazy_config_eval := true;
  Lazy.Mutexed.force Frame.duration

let events_m = Mutex.create ()
let events : Clock.Event.event list ref = ref []

let () =
  Clock.on_event (fun event ->
      Mutex.protect events_m (fun () -> events := event :: !events))

let clear_events () = Mutex.protect events_m (fun () -> events := [])
let all_events () = Mutex.protect events_m (fun () -> List.rev !events)
let event_count () = List.length (all_events ())

let events_of clock =
  List.filter_map
    (fun (event : Clock.Event.event) ->
      if event.clock = Clock.name clock then Some event.kind else None)
    (all_events ())

let live_sources = Atomic.make 0
let subscriptions = Atomic.make 0

class source ?sync ~id (source_type : [ `Passive | `Active | `Output ]) =
  object (self)
    val mutable sync : Clock.Sync_source.t option = sync
    val mutable subscribers : (Clock.Sync_source.t option -> unit) list = []
    val awake = Atomic.make 0
    val animated = Atomic.make 0
    val resets = Atomic.make 0
    val queries = Atomic.make 0
    val mutable on_animate : unit -> unit = ignore
    val mutable on_sleep : unit -> unit = ignore
    method id : string = id
    method stack : Pos.t list = []

    method source_type :
        [ `Passive | `Active of Clock.active | `Output of Clock.active ] =
      let active =
        object
          method id = id
          method reset = self#reset
          method output = self#animate
        end
      in
      match source_type with
        | `Passive -> `Passive
        | `Active -> `Active active
        | `Output -> `Output active

    method wake_up (requester : Clock.source) : Clock.activation =
      Atomic.incr awake;
      object
        method id = requester#id
      end

    method sleep (_ : Clock.activation) =
      Atomic.decr awake;
      on_sleep ()

    method animate : unit =
      Atomic.incr animated;
      on_animate ()

    method reset : unit = Atomic.incr resets

    method sync_source =
      Atomic.incr queries;
      sync

    method on_sync_source fn =
      Atomic.incr subscriptions;
      subscribers <- fn :: subscribers;
      fun () ->
        if List.memq fn subscribers then begin
          Atomic.decr subscriptions;
          subscribers <- List.filter (fun other -> other != fn) subscribers
        end

    method activations : Clock.activation list =
      if Atomic.get awake > 0 then
        [
          object
            method id = id
          end;
        ]
      else []

    method awake = Atomic.get awake
    method animated = Atomic.get animated
    method resets = Atomic.get resets
    method queries = Atomic.get queries
    method subscribers = List.length subscribers
    method set_on_animate fn = on_animate <- fn
    method set_on_sleep fn = on_sleep <- fn

    method set_sync value =
      sync <- value;
      List.iter (fun fn -> fn value) subscribers

    initializer
      Atomic.incr live_sources;
      Gc.finalise
        (fun source ->
          Atomic.decr live_sources;
          ignore (Atomic.fetch_and_add subscriptions (-source#subscribers)))
        self
  end

let source ?sync ?(id = "source") source_type = new source ?sync ~id source_type
let as_source source = (source :> Clock.source)
let attach clock source = Clock.attach clock (as_source source)
let detach clock source = Clock.detach clock (as_source source)
let owners = Atomic.make 0

let owner () =
  { Clock.kind = "test"; id = string_of_int (Atomic.fetch_and_add owners 1) }

(* A passive clock ticked by hand keeps a check free of animators and of real
   time. *)
let passive ?id ?on_error () =
  let clock = Clock.create ?id ?on_error ~sync:`Passive ~owner:(owner ()) () in
  Clock.start ~force:true clock;
  clock

let sub_clock ?id parent = Clock.create ?id ~sync:`Passive ~parent ()

let tick ?pull clock n =
  for _ = 1 to n do
    Clock.tick ?pull clock
  done

let is_stopped clock reason =
  match Clock.stop_reason clock with
    | Some found -> found = reason
    | None -> false

let failed clock =
  match Clock.stop_reason clock with Some (`Failed _) -> true | _ -> false

let registered clock =
  List.length (List.filter (Clock.equal clock) (Clock.clocks ()))

let collect () =
  Gc.full_major ();
  Gc.full_major ()

exception Boom

let policy_calls : (string * exn) list ref = ref []

let () =
  Clock.set_failure_policy (fun clock (failure : Clock.failure) ->
      policy_calls := (Clock.name clock, failure.error) :: !policy_calls)

let policy_calls_for clock =
  List.length
    (List.filter (fun (name, _) -> name = Clock.name clock) !policy_calls)
