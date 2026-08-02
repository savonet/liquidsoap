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

let messages = Atomic.make []

(* Add a startup message. *)
let message fmt =
  Printf.ksprintf (fun s -> Atomic.set messages (s :: Atomic.get messages)) fmt

(* Time a startup function. *)
let time name f =
  let t = Sys.time () in
  let ans = f () in
  message "%s: %.02fs" name (Sys.time () -. t);
  ans

let messages () = Atomic.get messages |> List.rev

(* Should we register external plugins? *)
let register_external_plugins = ref true
