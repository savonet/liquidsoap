(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details, fully stated in the COPYING
  file at the root of the liquidsoap distribution.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

let[@inline never] mutexify m f x =
  Mutex.lock m;
  match f x with
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        Mutex.unlock m;
        Printexc.raise_with_backtrace exn bt
    | v ->
        Mutex.unlock m;
        v

type state = {
  mutex : Mutex.t;
  condition : Condition.t;
  lock : [ `Locked | `Mutating | `None ] Atomic.t;
}

let mk_state () =
  {
    mutex = Mutex.create ();
    condition = Condition.create ();
    lock = Atomic.make `None;
  }

let[@inline always] on_mutex_done state =
  Mutex.lock state.mutex;
  Condition.broadcast state.condition;
  Atomic.set state.lock `None;
  Mutex.unlock state.mutex

(* The recheck under the mutex is what keeps the wait from being lost: the
   holder can release between the read above and this lock, and only a
   [`Mutating] holder ever broadcasts. *)
let rec wait state held =
  if not (Atomic.compare_and_set state.lock `None held) then (
    (match Atomic.get state.lock with
      | `Mutating ->
          mutexify state.mutex
            (fun () ->
              if Atomic.get state.lock = `Mutating then
                Condition.wait state.condition state.mutex)
            ()
      | _ -> Domain.cpu_relax ());
    wait state held)

let mutable_lock ~state fn v =
  wait state `Mutating;
  try
    let v = fn v in
    on_mutex_done state;
    v
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    on_mutex_done state;
    Printexc.raise_with_backtrace exn bt

let atomic_lock ~state fn v =
  wait state `Locked;
  try
    let v = fn v in
    Atomic.set state.lock `None;
    v
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    Atomic.set state.lock `None;
    Printexc.raise_with_backtrace exn bt

type reentrant = { state : state; owner : int Atomic.t }

let mk_reentrant () = { state = mk_state (); owner = Atomic.make (-1) }

(* Reading [owner] outside the lock is safe: no other thread can leave this
   thread's own id there. *)
let reentrant_lock ~fast ~state fn v =
  let self = Thread.id (Thread.self ()) in
  if Atomic.get state.owner = self then fn v
  else (
    let lock = if fast then atomic_lock else mutable_lock in
    lock ~state:state.state
      (fun v ->
        Atomic.set state.owner self;
        Fun.protect
          ~finally:(fun () -> Atomic.set state.owner (-1))
          (fun () -> fn v))
      v)
