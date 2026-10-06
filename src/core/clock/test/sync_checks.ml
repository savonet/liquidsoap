open Harness

let device name = Clock.Sync_source.make ~name `Self_paced

let tracked clock =
  Option.map
    (fun (sync : Clock.Status.sync) -> sync.sync_source)
    (Clock.status clock).sync

let sync_error = function Clock.Sync_error _ -> true | _ -> false

let identity () =
  let first = device "card" and second = device "card" in
  check "K4: two sync sources are the same exactly when their identities are"
    (Clock.Sync_source.equal first first
    && not (Clock.Sync_source.equal first second))

let one_query_per_source () =
  let clock = passive () in
  let sources = List.init 50 (fun _ -> source `Active) in
  List.iter (attach clock) sources;
  Clock.tick clock;
  let queries () =
    List.fold_left (fun total source -> total + source#queries) 0 sources
  in
  let after_activation = queries () in
  tick clock 1000;
  check "K1: a tick queries each animated source once for its pacer"
    (queries () - after_activation = 1000 * List.length sources)

let changes () =
  let clock = passive () in
  let pacer = source ~id:"pacer" `Active in
  let trigger = source ~id:"trigger" `Output in
  attach clock trigger;
  attach clock pacer;
  Clock.tick clock;
  check "K8: a clock whose sources report no pacer tracks none"
    (tracked clock = None);
  let card = device "card" in
  let during = ref (Some "unread") in
  trigger#set_on_animate (fun () ->
      Thread.join (Thread.create (fun () -> pacer#set_sync (Some card)) ());
      Clock.after_tick clock (fun () -> during := tracked clock);
      trigger#set_on_animate ignore);
  Clock.tick clock;
  check
    "a change reported from another thread during a tick is not applied in it"
    (!during = None);
  Clock.tick clock;
  check "K6: it is applied at the next tick" (tracked clock = Some "card");
  pacer#set_sync None;
  pacer#set_sync (Some (device "other"));
  Clock.tick clock;
  check "of two changes of one source before a tick, the later one applies"
    (tracked clock = Some "other");
  pacer#set_sync None;
  Clock.tick clock;
  check "K6: a pacer that goes away is dropped within one tick"
    (tracked clock = None)

let moving_together () =
  let handled = ref 0 in
  let clock = passive ~on_error:(fun _ _ -> incr handled) () in
  let old_card = device "old" and new_card = device "new" in
  let left = source ~sync:old_card `Active in
  let right = source ~sync:old_card `Active in
  attach clock left;
  attach clock right;
  Clock.tick clock;
  left#set_sync (Some new_card);
  right#set_sync (Some new_card);
  Clock.tick clock;
  check "two sources moving together to another sync source are no sync error"
    (!handled = 0 && tracked clock = Some "new")

let conflict_with_handler () =
  let errors = ref [] in
  let clock =
    passive ~id:"two.cards"
      ~on_error:(fun error _ -> errors := error :: !errors)
      ()
  in
  let first = source ~id:"first" ~sync:(device "card.1") `Active in
  let second = source ~id:"second" `Active in
  attach clock first;
  attach clock second;
  Clock.tick clock;
  second#set_sync (Some (device "card.2"));
  tick clock 2;
  check "a second sync source is a sync error naming both and their sources"
    (match !errors with
      | [Clock.Sync_error { clock = "two.cards"; reported }] ->
          List.sort compare
            (List.map
               (fun (r : Clock.reported) -> (r.sync_source, r.source))
               reported)
          = [("card.1", "first"); ("card.2", "second")]
      | _ -> false);
  check "it is charged to the source that brought the second one"
    (second#animated = 1 && first#animated = 3 && tracked clock = Some "card.1")

let conflict_without_handler () =
  let clock = passive () in
  attach clock (source ~id:"dup" ~sync:(device "card.1") `Active);
  attach clock (source ~id:"dup" ~sync:(device "card.2") `Active);
  raises "two sources with one id and two sync sources are two entries"
    sync_error (fun () -> Clock.tick clock);
  check "without a handler a sync error fails the clock" (failed clock)

let run () =
  identity ();
  one_query_per_source ();
  changes ();
  moving_together ();
  conflict_with_handler ();
  conflict_without_handler ()
