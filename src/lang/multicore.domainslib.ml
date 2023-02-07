(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

let name = "domainsLib"
let multicore = Atomic.make true
let domains_count = Atomic.make (Domain.recommended_domain_count () - 1)
let lazy_m = Mutex.create ()

let mutexify lock f x =
  let after =
    try
      Mutex.lock lock;
      fun () -> Mutex.unlock lock
    with Sys_error _ -> fun () -> ()
  in
  try
    let ans = f x in
    after ();
    ans
  with e ->
    let bt = Printexc.get_raw_backtrace () in
    after ();
    Printexc.raise_with_backtrace e bt

let force v =
  if Atomic.get multicore then mutexify lazy_m Lazy.force v else Lazy.force v

let pool =
  lazy
    (Domainslib.Task.setup_pool ~num_domains:(Atomic.get domains_count - 1) ())

let pool () = Lazy.force pool

let iter fn l =
  if Atomic.get multicore then (
    let pool = pool () in
    let l = Array.of_list l in
    Domainslib.Task.run pool (fun _ ->
        Domainslib.Task.parallel_for ~start:0
          ~finish:(Array.length l - 1)
          ~body:(fun pos -> fn l.(pos))
          pool))
  else List.iter fn l

let fold ~reconcile fn v l =
  if Atomic.get multicore then (
    let pool = pool () in
    let l = Array.of_list l in
    Domainslib.Task.run pool (fun _ ->
        Domainslib.Task.parallel_for_reduce ~start:0
          ~finish:(Array.length l - 1)
          ~body:(fun pos -> fn l.(pos))
          pool reconcile v))
  else List.fold_left (fun s x -> reconcile s (fn x)) v l

module Stack = struct
  type t = Mutex.t

  let create = Mutex.create
  let queue m fn = mutexify m fn ()
end
