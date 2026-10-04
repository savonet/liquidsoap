open Settings

type t = {
  notified : bool Atomic.t;
  resume : (unit -> unit) Atomic.t;
  m : Mutex.t;
  c : Condition.t;
  parks : int Atomic.t;
}

let create () =
  {
    notified = Atomic.make false;
    resume = Atomic.make ignore;
    m = Mutex.create ();
    c = Condition.create ();
    parks = Atomic.make 0;
  }

let wake parker =
  Atomic.set parker.notified true;
  Mutex.protect parker.m (fun () -> Condition.broadcast parker.c)

let unpark parker =
  (Atomic.get parker.resume) ();
  wake parker

(* The task of an earlier park finds [parks] moved on and does nothing. *)
let wake_after parker delay =
  let park = Atomic.get parker.parks in
  Duppy.Task.add scheduler
    {
      Duppy.Task.priority = `Non_blocking;
      events = [`Delay delay];
      handler =
        (fun _ ->
          if Atomic.get parker.parks = park then wake parker;
          []);
    }

let park_thread ?delay parker =
  Option.iter (wake_after parker) delay;
  Mutex.protect parker.m (fun () ->
      while not (Atomic.get parker.notified) do
        Condition.wait parker.c parker.m
      done);
  Atomic.incr parker.parks;
  Atomic.set parker.notified false

let park_task ?delay parker =
  if not (Atomic.get parker.notified) then
    Duppy.suspend ?delay ~priority:`Clock scheduler (fun resume ->
        Atomic.set parker.resume resume;
        if Atomic.get parker.notified then resume ());
  Atomic.set parker.resume ignore;
  Atomic.set parker.notified false
