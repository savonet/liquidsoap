open Harness

let count_events clock matching =
  List.length (List.filter matching (events_of clock))

let is_failure = function Clock.Event.Failure _ -> true | _ -> false

let without_handler () =
  let clock = passive ~id:"failing" () in
  let child = sub_clock clock in
  Clock.register ~parent:clock child;
  let good = source ~id:"good" `Output and bad = source ~id:"bad" `Output in
  attach clock good;
  attach clock bad;
  Clock.tick clock;
  bad#set_on_animate (fun () -> raise Boom);
  raises "the tick of a passive clock that fails raises the error" (( = ) Boom)
    (fun () -> Clock.tick clock);
  check "a failed clock is stopped with reason failed, the error and the source"
    (match Clock.stop_reason clock with
      | Some (`Failed { error = Boom; source = Some "bad" }) -> true
      | _ -> false);
  check "a failed clock is in the registry, its outputs asleep"
    (registered clock = 1 && good#awake = 0 && bad#awake = 0);
  check "the sub-clocks of a failed clock are stopped"
    (is_stopped child `Parent_stopped);
  check "the failure is logged once and reported to the policy once"
    (count_events clock is_failure = 1 && policy_calls_for clock = 1)

let with_handler () =
  let handled = ref [] in
  let clock =
    passive ~id:"handling"
      ~on_error:(fun error _ -> handled := error :: !handled)
      ()
  in
  let good = source ~id:"good" `Output and bad = source ~id:"bad" `Output in
  attach clock bad;
  attach clock good;
  Clock.tick clock;
  bad#set_on_animate (fun () -> raise Boom);
  tick clock 3;
  check "with a handler the tick completes and the clock carries on"
    (Clock.ticks clock = Some 4 && good#animated = 4);
  check "the failing source is detached and the handler called once"
    (bad#animated = 2 && bad#awake = 0 && !handled = [Boom]);
  check "the policy is not called" (policy_calls_for clock = 0);
  check "the source failure is logged as handled"
    (count_events clock (function
       | Clock.Event.Source_failure { source = "bad"; handled = true } -> true
       | _ -> false)
    = 1)

let callback () =
  let clock = passive ~id:"callback" () in
  Clock.on_tick clock (fun () -> raise Boom);
  raises "an error from a callback leaves the tick" (( = ) Boom) (fun () ->
      Clock.tick clock);
  check "an error from a callback fails the clock" (failed clock)

let sub_clock_under_parent () =
  let parent = passive ~id:"parent" () in
  let child = sub_clock ~id:"failing.child" parent in
  Clock.register ~parent child;
  let bad = source `Output in
  attach child bad;
  bad#set_on_animate (fun () -> raise Boom);
  tick parent 3;
  check "a sub-clock failing under its parent's tick lets the parent carry on"
    (Clock.ticks parent = Some 3 && failed child);
  check "a failed sub-clock is reported once" (policy_calls_for child = 1);
  raises "a later pull of a failed sub-clock meets not running"
    (function Clock.Not_running _ -> true | _ -> false)
    (fun () -> Clock.tick ~pull:true child)

let sub_clock_under_pull () =
  let handled = ref [] in
  let parent =
    passive ~id:"reading"
      ~on_error:(fun error _ -> handled := error :: !handled)
      ()
  in
  let child = sub_clock ~id:"pulled.child" parent in
  Clock.register ~parent child;
  let bad = source `Output in
  attach child bad;
  bad#set_on_animate (fun () -> raise Boom);
  let reader = source ~id:"reader" `Output in
  attach parent reader;
  reader#set_on_animate (fun () -> Clock.tick ~pull:true child);
  tick parent 2;
  check "a sub-clock failing under a pull passes the error to its reader"
    (!handled = [Boom] && reader#awake = 0 && failed child);
  check "the parent, which has a handler, carries on"
    (Clock.ticks parent = Some 2)

let run () =
  without_handler ();
  with_handler ();
  callback ();
  sub_clock_under_parent ();
  sub_clock_under_pull ()
