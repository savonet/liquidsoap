(* Each body the scheduler runs registers a script callback, which raises
   [Effect.Unhandled] on a thread that has no handler for it. *)

let registered = Atomic.make 0

let register () =
  Script_callback.notify ~owner:0 ignore;
  Atomic.incr registered

let task ?(then_ = []) () =
  {
    Scheduler.Task.priority = `Threaded;
    events = [`Delay 0.];
    handler =
      (fun _ ->
        register ();
        then_);
  }

let wait_for count =
  let deadline = Unix.gettimeofday () +. 5. in
  while Atomic.get registered < count && Unix.gettimeofday () < deadline do
    Thread.delay 0.01
  done;
  if Atomic.get registered <> count then (
    Printf.printf "%d registrations out of %d\n%!" (Atomic.get registered) count;
    exit 1)

let () =
  Dtools.Log.conf_stdout#set true;
  Dtools.Log.conf_file#set false;
  Dtools.Init.exec Dtools.Log.start;
  Scheduler.start ();
  Scheduler.Task.add (task ~then_:[task ()] ());
  wait_for 2;
  let async =
    Scheduler.Async.add ~priority:`Threaded (fun () ->
        register ();
        -1.)
  in
  Scheduler.Async.wake_up async;
  wait_for 3;
  Scheduler.Task.add
    {
      (task ()) with
      handler =
        (fun _ ->
          Scheduler.run (fun () ->
              register ();
              Scheduler.reschedule ~priority:`Non_blocking ();
              register ());
          []);
    };
  wait_for 5;
  Dtools.Init.exec Dtools.Log.stop;
  Tutils.shutdown 0
