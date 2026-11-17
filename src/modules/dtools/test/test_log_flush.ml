(* [flush] must wait for a batch another thread already took off the queue: a
   custom log blocks printing that batch, and a concurrent [flush] must not
   return before it is released. *)

let mutex = Mutex.create ()
let changed = Condition.create ()
let in_hook = ref false
let released = ref false
let second_flush_done = ref false
let with_lock f = Mutex.protect mutex f

let wait_for cond =
  with_lock (fun () ->
      while not (cond ()) do
        Condition.wait changed mutex
      done)

let set r =
  with_lock (fun () ->
      r := true;
      Condition.broadcast changed)

let () =
  Dtools.Log.add_custom_log "blocking"
    {
      Dtools.Log.timestamp = false;
      exec =
        (fun _ ->
          set in_hook;
          wait_for (fun () -> !released));
    };
  let log = Dtools.Log.make ["test"] in
  log#f 1 "entry";
  let first = Thread.create Dtools.Log.flush () in
  wait_for (fun () -> !in_hook);
  let second =
    Thread.create
      (fun () ->
        Dtools.Log.flush ();
        set second_flush_done)
      ()
  in
  (* Long enough for a [flush] that does not wait to return. *)
  Thread.delay 0.5;
  if with_lock (fun () -> !second_flush_done) then (
    prerr_endline "flush returned while another thread was printing";
    exit 1);
  set released;
  Thread.join first;
  Thread.join second;
  assert !second_flush_done;
  print_endline "ok"
