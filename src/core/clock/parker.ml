open Settings

(* Where a clock's loop rests, as a suspended task or as a waiting thread, and
   where it is woken early. *)
type t = {
  (* Set by a wake-up and consumed by the park it ends. *)
  notified : bool Atomic.t;
  (* Resumes the task parked here; [ignore] when none is. *)
  resume : (unit -> unit) Atomic.t;
  m : Mutex.t;
  c : Condition.t;
  (* Number of thread parks completed, which dates a delayed wake-up. *)
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

(* Ends the park in progress, or the next one.

   [notified] is set before the resumer is read: a task that is just parking
   either gets resumed here or sees the flag. *)
let unpark parker =
  Atomic.set parker.notified true;
  (Atomic.get parker.resume) ();
  wake parker

(* Ends the thread park in progress after [delay].

   The task of an earlier park finds [parks] moved on and does nothing. *)
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

(* Blocks the calling thread until a wake-up, or [delay] at most. *)
let park_thread ?delay parker =
  Option.iter (wake_after parker) delay;
  Mutex.protect parker.m (fun () ->
      while not (Atomic.get parker.notified) do
        Condition.wait parker.c parker.m
      done);
  Atomic.incr parker.parks;
  Atomic.set parker.notified false

(* Suspends the calling task until a wake-up, or [delay] at most, and frees its
   worker. *)
let park_task ?delay parker =
  if not (Atomic.get parker.notified) then
    Duppy.suspend ?delay ~priority:`Clock scheduler (fun resume ->
        Atomic.set parker.resume resume;
        if Atomic.get parker.notified then resume ());
  Atomic.set parker.resume ignore;
  Atomic.set parker.notified false
