(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let loader = ref (fun () -> ())
let keywords : (string, unit -> unit) Hashtbl.t = Hashtbl.create 10

let register ?keyword f =
  let f =
    let already = ref false in
    fun () ->
      if not !already then (
        already := true;
        f ())
  in
  let prev = !loader in
  (loader :=
     fun () ->
       prev ();
       f ());
  match keyword with None -> () | Some k -> Hashtbl.add keywords k f

let trigger k =
  match Hashtbl.find_opt keywords k with Some f -> f () | None -> ()

let load () = !loader ()
