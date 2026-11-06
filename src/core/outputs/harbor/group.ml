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
let discard = Domain.DLS.new_key (fun () -> Bytes.create 4096)

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

let has_members t = match Atomic.get t.members with [] -> false | _ -> true

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
    (match Listener.deadline listener with
      | Some deadline when now >= deadline && not (Listener.is_closed listener)
        ->
          t.log#info "Listener %s reached its time limit"
            (Listener.info listener).peer;
          Listener.mark_closed listener
      | _ -> ());
    if (not (Listener.is_closed listener)) && member.pending () then (
      if Listener.owed_nothing listener then
        Listener.set_last_progress listener now;
      let sent = Listener.bytes_sent listener in
      member.flush ~scratch;
      if Listener.bytes_sent listener > sent then
        Listener.set_last_progress listener now;
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
  let members = Atomic.get t.members in
  let is_closed m = Listener.is_closed m.listener in
  if not (List.exists is_closed members) then []
  else (
    let closed, live = List.partition is_closed members in
    Atomic.set t.members live;
    closed)

let release removed =
  List.iter
    (fun m -> if Listener.release_socket m.listener `Owner then teardown m)
    removed

(* Waits for readability, so that a listener owed nothing is still seen
   hanging up; what it sends is discarded. It holds the socket until it ends:
   the owner's release shuts the socket down to wake it, and the delay ends it
   where that shutdown wakes nothing. *)
let rec watch_hangup member =
  let listener = member.listener in
  let fd = (Listener.socket listener)#file_descr in
  let finish () =
    if Listener.release_socket listener `Watch then teardown member;
    []
  in
  {
    Scheduler.Task.priority = `Non_blocking;
    events = [`Read fd; `Delay retry_interval];
    handler =
      (fun events ->
        if Listener.is_closed listener then finish ()
        else if not (List.mem (`Read fd) events) then [watch_hangup member]
        else (
          let buffer = Domain.DLS.get discard in
          match Unix.read fd buffer 0 (Bytes.length buffer) with
            | 0 ->
                Listener.mark_closed listener;
                finish ()
            | _ -> [watch_hangup member]
            | exception Unix.Unix_error (error, _, _)
              when Liq_http.would_block error || error = Unix.EINTR ->
                [watch_hangup member]
            | exception _ ->
                Listener.mark_closed listener;
                finish ()));
  }

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
          let next =
            List.fold_left
              (fun next m ->
                let owed = m.pending () in
                Listener.set_owed_nothing m.listener (not owed);
                match next with
                  | `Wake -> `Wake
                  | _ when not owed -> next
                  | _ when Listener.last_progress m.listener >= now -> `Wake
                  | _ -> `Retry)
              `Idle (Atomic.get t.members)
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
          Listener.hold_socket member.listener;
          Atomic.set t.members (member :: Atomic.get t.members);
          true))
  in
  if published then (
    Scheduler.Task.add (watch_hangup member);
    wake t;
    (* The deadline is checked by passes, which only data or a retry start. *)
    Option.iter
      (fun deadline ->
        add_task
          ~delay:(max 0. (deadline -. Listener.now ()))
          (fun () -> wake t))
      (Listener.deadline member.listener));
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
