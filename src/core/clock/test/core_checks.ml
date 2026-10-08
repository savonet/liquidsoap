open Harness

let lifecycle () =
  let before = event_count () in
  let clock = Clock.create ~sync:`Passive ~owner:(owner ()) () in
  check "a new clock is stopped, never started"
    (is_stopped clock `Never_started);
  check "a stopped clock has no ticks, no time and no lateness"
    (Clock.ticks clock = None
    && Clock.time clock = None
    && lateness clock = None
    && not (Clock.self_sync clock));
  check "reading a stopped clock logs nothing" (event_count () = before);
  Clock.start ~force:true clock;
  check "a forced start leaves a passive clock started with 0 ticks"
    (Clock.started clock && Clock.ticks clock = Some 0);
  let output = source ~id:"out" `Output in
  attach clock output;
  Clock.activate_pending clock;
  check "an activated output is listed"
    (List.map
       (fun (entry : Clock.Status.entry) -> entry.id)
       (Option.get (run clock)).outputs
    = ["out"]);
  tick clock 7;
  check "each tick adds exactly one to the tick count"
    (Clock.ticks clock = Some 7);
  check "stream time is ticks times the frame duration, exactly"
    (Clock.time clock = Some (7. *. frame_duration));
  let order = ref [] in
  Clock.after_tick clock (fun () -> order := "after" :: !order);
  Clock.on_tick clock (fun () -> order := "on" :: !order);
  tick clock 2;
  check "an on-tick callback runs before an after-tick one, each once"
    (List.rev !order = ["on"; "after"]);
  Clock.stop clock;
  check "stopping a passive clock that nothing ticks is immediate"
    (is_stopped clock `Requested && Clock.ticks clock = None);
  let before = event_count () in
  raises "ticking a stopped clock fails with not running"
    (function Clock.Not_running _ -> true | _ -> false)
    (fun () -> Clock.tick clock);
  check "ticking a stopped clock logs nothing" (event_count () = before)

let stop_inside_tick () =
  let clock = passive () in
  let seen_started = ref false in
  Clock.on_tick clock (fun () ->
      Clock.stop clock;
      seen_started := Clock.ticks clock <> None);
  Clock.tick clock;
  check "a stop from inside a tick winds the clock down at the end of it"
    (!seen_started && is_stopped clock `Requested)

let callback_during_stop () =
  let clock = passive () in
  let registered = ref false in
  Clock.on_tick clock (fun () ->
      Clock.stop clock;
      Clock.on_tick clock ignore;
      Clock.after_tick clock ignore;
      registered := true);
  (try Clock.tick clock with _ -> ());
  check "a callback registered from a tick that a stop landed in is accepted"
    (!registered && is_stopped clock `Requested)

let failed_start () =
  let clock = Clock.create ~id:"unstartable" () in
  attach clock (source ~id:"idle" `Passive);
  let before = Clock.status clock in
  let registered_before = registered clock in
  raises "a clock with no output cannot start"
    (function
      | Clock.Cannot_start { reason = `Application_not_started | `No_output } ->
          true
      | _ -> false)
    (fun () -> Clock.start clock);
  check "a start that cannot proceed leaves the clock as it was"
    (Clock.status clock = before && registered clock = registered_before)

let registry () =
  let top = Clock.create () in
  check "a stopped top-level clock is in the registry once" (registered top = 1);
  let parent = passive () in
  check "a started top-level clock is in the registry once"
    (registered parent = 1);
  let sub = sub_clock parent in
  Clock.register ~parent sub;
  check "a clock with a parent is not in the registry" (registered sub = 0);
  Clock.deregister ~parent sub;
  check "a clock with a parent is not in the registry after it stops"
    (is_stopped sub `Parent_stopped && registered sub = 0);
  Clock.stop parent;
  check "a stopped clock stays in the registry once" (registered parent = 1);
  let created = ref None in
  raises "a start scope passes on the error of its function" (( = ) Boom)
    (fun () ->
      Clock.start_scope (fun () ->
          let clock = Clock.create () in
          attach clock (source `Output);
          created := Some clock;
          raise Boom));
  check "a start scope whose function fails starts nothing"
    (is_stopped (Option.get !created) `Never_started)

(* The allocation is opaque so that flambda does not keep the clocks alive. *)
let discarded_clocks count =
  for _ = 1 to count do
    ignore (Sys.opaque_identity (Clock.create ()))
  done

let start_pass () =
  collect ();
  let baseline = Clock.start_pass () in
  let held = List.init 100 (fun _ -> Clock.create ()) in
  check "K5: a start pass examines each waiting clock once"
    (Clock.start_pass () = baseline + 100);
  ignore (Sys.opaque_identity held);
  discarded_clocks 1000;
  collect ();
  check "clocks created and discarded do not accumulate"
    (Clock.start_pass () <= baseline + 100)

let discarded_sources_return () =
  let clock = passive () in
  attach clock (source ~id:"keep" `Output);
  Clock.tick clock;
  collect ();
  let sources = Atomic.get live_sources in
  for _ = 1 to 200 do
    attach clock (Sys.opaque_identity (source `Active));
    attach clock (Sys.opaque_identity (source `Passive));
    Clock.tick clock
  done;
  collect ();
  Clock.tick clock;
  collect ();
  check "K2: discarded sources are not kept alive by a running clock"
    (Atomic.get live_sources = sources)

let output_stays_awake () =
  let clock = passive () in
  let output = source `Output in
  attach clock output;
  tick clock 3;
  check "K17: an output stays awake while its clock runs" (output#awake = 1);
  Clock.stop clock;
  check "K17: winding down puts it to sleep once per activation"
    (output#awake = 0)

let detach_during_tick () =
  let clock = passive () in
  let first = source ~id:"first" `Output in
  let second = source ~id:"second" `Output in
  let third = source ~id:"third" `Output in
  List.iter (attach clock) [first; second; third];
  Clock.tick clock;
  first#set_on_animate (fun () -> detach clock third);
  Clock.tick clock;
  check "a detach during a tick only takes the detached source out of it"
    (first#animated = 2 && second#animated = 2 && third#animated = 1);
  Clock.tick clock;
  check "a detached source is not animated again"
    (third#animated = 1 && third#awake = 0)

let leak_warning () =
  let clock = passive () in
  let warnings () =
    List.length
      (List.filter
         (function Clock.Event.Leak_warning _ -> true | _ -> false)
         (events_of clock))
  in
  let threshold = 50 in
  for _ = 1 to threshold - 1 do
    attach clock (source `Passive)
  done;
  Clock.tick clock;
  let quiet = warnings () = 0 in
  attach clock (source `Passive);
  attach clock (source `Passive);
  Clock.tick clock;
  Clock.tick clock;
  check "crossing a multiple of the leak threshold logs one warning"
    (quiet && warnings () = 1)

let nested_ticks () =
  let parent = passive () in
  let child = sub_clock parent in
  let grandchild = sub_clock child in
  Clock.register ~parent child;
  Clock.register ~parent:child grandchild;
  check "registering on a started parent starts the sub-clock, at every depth"
    (Clock.started child && Clock.started grandchild);
  tick parent 3;
  check "a tick of the parent produces nothing in a sub-clock nobody ticks"
    (Clock.ticks child = Some 0 && Clock.ticks grandchild = Some 0);
  let reader = source `Output in
  attach parent reader;
  let pulls = ref [3; 0; 0] in
  reader#set_on_animate (fun () ->
      match !pulls with
        | n :: rest ->
            pulls := rest;
            tick child n
        | [] -> ());
  tick parent 3;
  check "K19: a sub-clock ticks as often as its reader ticks it"
    (Clock.ticks child = Some 3 && Clock.ticks grandchild = Some 0);
  let inner = source `Output and below = source `Output in
  let closed = ref false in
  reader#set_on_animate (fun () ->
      attach child inner;
      attach grandchild below;
      Clock.after_tick grandchild (fun () -> closed := true));
  tick parent 1;
  check "K19: a tick cleans up the sub-clocks at every depth"
    (inner#awake = 0 && below#awake = 0 && !closed);
  reader#set_on_animate ignore;
  tick parent 1;
  check "K19: a tick prepares the sub-clocks at every depth"
    (inner#awake = 1 && below#awake = 1);
  check "K19: a sub-clock nobody ticks produces nothing and counts no tick"
    (inner#animated = 0 && below#animated = 0
    && Clock.ticks child = Some 3
    && Clock.ticks grandchild = Some 0);
  Clock.stop grandchild;
  tick parent 1;
  check "a stopped sub-clock is skipped and the parent's tick succeeds"
    (Clock.ticks parent = Some 9 && Clock.started child)

let registration () =
  let parent = passive () in
  let child = sub_clock parent in
  Clock.register ~parent child;
  Clock.register ~parent child;
  Clock.deregister ~parent child;
  check "with two registrants, the first deregistration changes nothing"
    (Clock.started child && List.length (Clock.sub_clocks parent) = 1);
  Clock.deregister ~parent child;
  check "the last deregistration stops the sub-clock"
    (is_stopped child `Parent_stopped && Clock.sub_clocks parent = []);
  let not_a_sub = function Clock.Not_a_sub_clock _ -> true | _ -> false in
  raises "a clock that is not passive cannot be registered" not_a_sub (fun () ->
      Clock.register ~parent (Clock.create ()));
  raises "a passive clock cannot be registered on another clock than its parent"
    not_a_sub (fun () -> Clock.register ~parent:(passive ()) child);
  for _ = 1 to 100 do
    let sub = sub_clock parent in
    Clock.register ~parent sub;
    Clock.tick parent;
    Clock.deregister ~parent sub
  done;
  check "K3: the entry count returns to its starting value"
    (Clock.sub_clocks parent = [])

let sleeping_output_deregisters () =
  let parent = passive () in
  let child = sub_clock parent in
  let output = source `Output in
  attach parent output;
  Clock.register ~parent child;
  Clock.tick parent;
  output#set_on_sleep (fun () -> Clock.deregister ~parent child);
  Clock.stop parent;
  check "K3: a sub-clock deregistered by an output going to sleep ends stopped"
    (Clock.stop_reason child <> None && Clock.stop_reason parent <> None)

let controllers () =
  let invalid = function Invalid_argument _ -> true | _ -> false in
  raises "K15: a passive clock cannot be created without a controller" invalid
    (fun () -> Clock.create ~sync:`Passive ());
  raises "a clock that is not passive cannot be given a controller" invalid
    (fun () -> Clock.create ~owner:(owner ()) ());
  raises "any other text than the four sync modes fails to parse" invalid
    (fun () -> Clock.sync_mode_of_string "self");
  check "the four sync mode names are auto, cpu, none and passive"
    (List.map Clock.string_of_sync_mode
       (List.map Clock.sync_mode_of_string ["auto"; "cpu"; "none"; "passive"])
    = ["auto"; "cpu"; "none"; "passive"])

let names () =
  let clock = Clock.create () in
  check "a clock with nothing is named generic" (Clock.name clock = "generic");
  attach clock (source ~id:"quiet" `Passive);
  attach clock (source ~id:"loud" `Output);
  check "a clock is named after its most significant pending source"
    (Clock.name clock = "loud");
  Clock.set_id clock "mine";
  let other = Clock.create ~id:"mine" () in
  check "an id is made unique among clock ids"
    (Clock.name clock = "mine" && Clock.name other <> "mine")

let restart () =
  let parent = passive () in
  let child = sub_clock parent in
  let output = source ~id:"out" `Output in
  let detached = source ~id:"detached" `Output in
  attach child output;
  attach child detached;
  Clock.register ~parent child;
  tick child 3;
  Clock.deregister ~parent child;
  check "a stopped sub-clock has put its outputs to sleep"
    (is_stopped child `Parent_stopped && output#awake = 0);
  detach child detached;
  let animated = output#animated and left_out = detached#animated in
  Clock.register ~parent child;
  check "a sub-clock registered again starts from 0 ticks"
    (Clock.started child && Clock.ticks child = Some 0);
  tick child 2;
  check "a restarted clock animates the output it held when it stopped"
    (Clock.ticks child = Some 2
    && output#awake = 1
    && output#animated = animated + 2);
  check "a source detached while the clock was stopped stays out"
    (detached#awake = 0 && detached#animated = left_out)

let run () =
  lifecycle ();
  restart ();
  stop_inside_tick ();
  callback_during_stop ();
  failed_start ();
  registry ();
  start_pass ();
  discarded_sources_return ();
  output_stays_awake ();
  detach_during_tick ();
  leak_warning ();
  nested_ticks ();
  registration ();
  sleeping_output_deregisters ();
  controllers ();
  names ()
