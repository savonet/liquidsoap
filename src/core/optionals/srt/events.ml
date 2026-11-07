type queue = {
  name : string;
  m : Mutex.t;
  jobs : (unit -> unit) Queue.t;
  mutable runner : int option;
}

type t = { queue : queue; task : Scheduler.Async.t }

let log = Library.log

let run_job t job =
  try job ()
  with exn ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log ~bt
      (Printf.sprintf "%s: error while processing event: %s" t.name
         (Printexc.to_string exn))

let next t =
  Mutex.protect t.m (fun () ->
      match Queue.take_opt t.jobs with
        | Some job -> Some job
        | None ->
            t.runner <- None;
            None)

let drain t =
  Mutex.protect t.m (fun () -> t.runner <- Some (Thread.id (Thread.self ())));
  let rec loop () =
    match next t with
      | Some job ->
          run_job t job;
          loop ()
      | None -> ()
  in
  loop ();
  -1.

let create name =
  let queue =
    { name; m = Mutex.create (); jobs = Queue.create (); runner = None }
  in
  {
    queue;
    task = Scheduler.Async.add ~priority:`Threaded (fun () -> drain queue);
  }

let on_runner { queue } =
  Mutex.protect queue.m (fun () ->
      queue.runner = Some (Thread.id (Thread.self ())))

let post { queue; task } job =
  Mutex.protect queue.m (fun () -> Queue.push job queue.jobs);
  Scheduler.Async.wake_up task

let run t job =
  if on_runner t then job ()
  else (
    let m = Mutex.create () in
    let c = Condition.create () in
    let result = ref None in
    post t (fun () ->
        let r =
          try Ok (job ())
          with exn -> Error (exn, Printexc.get_raw_backtrace ())
        in
        Mutex.protect m (fun () ->
            result := Some r;
            Condition.signal c));
    Mutex.protect m (fun () ->
        while !result = None do
          Condition.wait c m
        done);
    match Option.get !result with
      | Ok v -> v
      | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt)
