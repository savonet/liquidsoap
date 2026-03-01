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

let mutable_lock ~state fn =
  let rec wait () =
    if not (Atomic.compare_and_set state.lock `None `Mutating) then (
      Domain.cpu_relax ();
      wait ())
  in
  wait ();
  Fun.protect fn ~finally:(fun () ->
      Mutex.lock state.mutex;
      Condition.signal state.condition;
      Atomic.set state.lock `None;
      Mutex.unlock state.mutex)

let atomic_lock ~state fn =
  let rec wait () =
    if not (Atomic.compare_and_set state.lock `None `Locked) then (
      (match Atomic.get state.lock with
        | `Mutating ->
            mutexify state.mutex
              (fun () -> Condition.wait state.condition state.mutex)
              ()
        | _ -> Domain.cpu_relax ());
      wait ())
  in
  wait ();
  Fun.protect fn ~finally:(fun () -> Atomic.set state.lock `None)
