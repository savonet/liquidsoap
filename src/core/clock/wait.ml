(** Waiting for work handed to the scheduler: spec/clock.md §9. *)

open Settings
open Status
open State
open Pacing

type t = { m : Mutex.t; c : Condition.t; wakers : (unit -> unit) list Atomic.t }

let create () =
  { m = Mutex.create (); c = Condition.create (); wakers = Atomic.make [] }

let blocked : t list Atomic.t = Atomic.make []

let rec remove queue value =
  let values = Atomic.get queue in
  if
    not
      (Atomic.compare_and_set queue values
         (List.filter (fun other -> other != value) values))
  then remove queue value

let signal wait =
  Mutex.protect wait.m (fun () -> Condition.broadcast wait.c);
  List.iter (fun waker -> waker ()) (Atomic.get wait.wakers)

let account st ~started =
  let real_time = Duppy.time () in
  let spent = real_time -. started in
  st.tick_waited <- st.tick_waited +. spent;
  st.worker_since <- real_time;
  update_figures st (fun figures ->
      { figures with waiting = figures.waiting +. spent })

let on_worker wait c st ready =
  let waker () = Parker.unpark st.parker in
  let activity = Atomic.get st.activity in
  push wait.wakers waker;
  Fun.protect
    ~finally:(fun () ->
      remove wait.wakers waker;
      Atomic.set st.activity activity)
    (fun () ->
      Atomic.set st.activity `Waiting;
      while (not (ready ())) && running c do
        Parker.park_task st.parker
      done)

let blocking wait context ready =
  let stopped () =
    Atomic.get global_stop
    || match context with Some (c, _) -> state c <> `Started | None -> false
  in
  let interrupt () = signal wait in
  push blocked wait;
  Option.iter (fun (_, st) -> Atomic.set st.interrupt_wait interrupt) context;
  Fun.protect
    ~finally:(fun () ->
      remove blocked wait;
      Option.iter (fun (_, st) -> Atomic.set st.interrupt_wait ignore) context)
    (fun () ->
      Mutex.protect wait.m (fun () ->
          while (not (ready ())) && not (stopped ()) do
            Condition.wait wait.c wait.m
          done))

(* A worker is only given back where the tick can resume elsewhere: not
     under the transition lock, which belongs to a thread. *)
let until wait ready =
  let started = Duppy.time () in
  let context = context () in
  (match context with
    | Some (c, st)
      when (match Atomic.get st.animator with
             | Some (`Task, _) -> true
             | _ -> false)
           && not (Transition.held ()) ->
        on_worker wait c st ready
    | _ -> blocking wait context ready);
  Option.iter (fun (_, st) -> account st ~started) context
