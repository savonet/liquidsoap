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

(** The Lazy module is not currently compatible with multithreading, see
    https://github.com/ocaml-multicore/ocaml-multicore/issues/750 For now, we
    mutexify all accesses by using this module. The interface is mostly the same
    as the one as the Lazy module.
*)

type 'a t = 'a Lazy.t * Mutex.t

let[@inline] from_lazy v : 'a t = (v, Mutex.create ())
let[@inline] from_val v : 'a t = from_lazy (Lazy.from_val v)
let[@inline] from_fun f : 'a t = from_lazy (Lazy.from_fun f)

let force ((v, m) : 'a t) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () -> Lazy.force v

let to_fun v () = force v

let is_val ((v, m) : 'a t) =
  Mutex.lock m;
  Fun.protect ~finally:(fun () -> Mutex.unlock m) @@ fun () -> Lazy.is_val v
