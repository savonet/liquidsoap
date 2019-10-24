(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

let register_plugins = ref (fun () -> ())

(** Register a function which will register plugins. *)
let register_plugin ?(fast=true) f =
  (* For now fast plugins are loaded immediately. *)
  if fast then f ()
  else
    let g = !register_plugins in
    register_plugins := (fun () -> g (); f ())

(** Ensure that all plugins are registered. This function can be called multiple
    times and will not do anything excepting the first time. *)
let register_plugins () =
  !register_plugins ();
  register_plugins := fun () -> ()
