type member = {
  listener : Listener.t;
  pending : unit -> bool;
  flush : scratch:bytes -> unit;
  teardown : unit -> unit;
}

type schedule = [ `Idle | `Scheduled | `Running | `Rewake ]

type t = {
  log : Log.t;
  timeout : float;
  lock : Mutex.t;
  members : member list Atomic.t;
  mutable stopping : bool;
  schedule : schedule Atomic.t;
  retry_armed : bool Atomic.t;
}

let retry_interval = 1.
let scratch_size = 131072
let scratch = Domain.DLS.new_key (fun () -> Bytes.create scratch_size)

let create ~log ~timeout () =
  {
    log;
    timeout;
    lock = Mutex.create ();
    members = Atomic.make [];
    stopping = false;
    schedule = Atomic.make `Idle;
    retry_armed = Atomic.make false;
  }

let has_members t = Atomic.get t.members <> []

let add_task ?(delay = 0.) handler =
  Scheduler.Task.add
    {
      Scheduler.Task.priority = `Non_blocking;
      events = [`Delay delay];
      handler =
        (fun _ ->
          handler ();
          []);
    }

let teardown member =
  Scheduler.Task.add
    {
      Scheduler.Task.priority = `Threaded;
      events = [`Delay 0.];
      handler =
        (fun _ ->
          member.teardown ();
          []);
    }

let serve t ~scratch ~now member =
  let listener = member.listener in
  try
    if (not (Listener.is_closed listener)) && member.pending () then (
      member.flush ~scratch;
      if
        (not (Listener.is_closed listener))
        && now -. Listener.last_progress listener > t.timeout
      then (
        t.log#info "Listener %s timed out (no progress for %.02fs)"
          (Listener.info listener).peer t.timeout;
        Listener.mark_closed listener))
  with exn ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log:t.log ~bt
      (Printf.sprintf "Error while serving listener %s: %s"
         (Listener.info listener).peer (Printexc.to_string exn));
    Listener.mark_closed listener

(* Must be called with [t.lock] held; returns the removed members, whose
   sockets the caller closes once the lock is released. *)
let remove_closed t =
  let closed, live =
    List.partition
      (fun m -> Listener.is_closed m.listener)
      (Atomic.get t.members)
  in
  Atomic.set t.members live;
  closed

let release removed =
  List.iter
    (fun m ->
      Listener.close_socket m.listener;
      teardown m)
    removed

let rec wake t =
  match Atomic.get t.schedule with
    | `Idle ->
        if Atomic.compare_and_set t.schedule `Idle `Scheduled then
          add_task (fun () -> run t)
        else wake t
    | `Running ->
        if not (Atomic.compare_and_set t.schedule `Running `Rewake) then wake t
    | `Scheduled | `Rewake -> ()

and arm_retry t =
  if Atomic.compare_and_set t.retry_armed false true then
    add_task ~delay:retry_interval (fun () ->
        Atomic.set t.retry_armed false;
        wake t)

and pass t =
  (* Stop holds the lock for the last pass; a pass that finds it taken has
     nothing left to do, and a publication wakes the group once it is out. *)
  if Mutex.try_lock t.lock then (
    let removed, next =
      Fun.protect
        ~finally:(fun () -> Mutex.unlock t.lock)
        (fun () ->
          let scratch = Domain.DLS.get scratch in
          let now = Listener.now () in
          List.iter (serve t ~scratch ~now) (Atomic.get t.members);
          let removed = remove_closed t in
          let owed =
            List.filter (fun m -> m.pending ()) (Atomic.get t.members)
          in
          let next =
            if
              List.exists
                (fun m -> Listener.last_progress m.listener >= now)
                owed
            then `Wake
            else if owed <> [] then `Retry
            else `Idle
          in
          (removed, next))
    in
    release removed;
    match next with `Wake -> wake t | `Retry -> arm_retry t | `Idle -> ())

and run t =
  Atomic.set t.schedule `Running;
  Fun.protect
    ~finally:(fun () ->
      if not (Atomic.compare_and_set t.schedule `Running `Idle) then (
        Atomic.set t.schedule `Scheduled;
        add_task (fun () -> run t)))
    (fun () -> pass t)

let publish t member =
  let published =
    Mutex.protect t.lock (fun () ->
        if t.stopping then false
        else (
          Listener.set_last_progress member.listener (Listener.now ());
          Atomic.set t.members (member :: Atomic.get t.members);
          true))
  in
  if published then wake t;
  published

let stop t =
  let removed =
    Mutex.protect t.lock (fun () ->
        t.stopping <- true;
        let scratch = Domain.DLS.get scratch in
        let now = Listener.now () in
        List.iter
          (fun m ->
            serve t ~scratch ~now m;
            Listener.mark_closed m.listener)
          (Atomic.get t.members);
        remove_closed t)
  in
  release removed
