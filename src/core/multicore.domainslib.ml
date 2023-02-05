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

let parallelism = ref false

let pool =
  Domainslib.Task.setup_pool
    ~num_domains:(Domain.recommended_domain_count ())
    ()

let iter fn l =
  if !parallelism then (
    let l = Array.of_list l in
    Domainslib.Task.run pool (fun _ ->
        Domainslib.Task.parallel_for ~start:0
          ~finish:(Array.length l - 1)
          ~body:(fun pos -> fn l.(pos))
          pool))
  else List.iter fn l

let fold ~reconcile fn v l =
  if !parallelism then (
    let l = Array.of_list l in
    Domainslib.Task.run pool (fun _ ->
        Domainslib.Task.parallel_for_reduce ~start:0
          ~finish:(Array.length l - 1)
          ~body:(fun pos -> fn l.(pos))
          pool reconcile v))
  else List.fold_left (fun s x -> reconcile s (fn x)) v l
