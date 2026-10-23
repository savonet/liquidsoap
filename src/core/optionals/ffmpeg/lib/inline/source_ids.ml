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

type t = {
  input : Source.source;
  id : Source_id.t;
  mutable elapsed : int;
  mutable is_open : bool;
  mutable users : int;
}

let records = ref []
let lock = Mutex.create ()

let join input =
  Mutex.protect lock (fun () ->
      match List.find_opt (fun r -> r.input == input && r.is_open) !records with
        | Some r ->
            r.users <- r.users + 1;
            r
        | None ->
            let r =
              {
                input;
                id = Source_id.make ();
                elapsed = 0;
                is_open = true;
                users = 1;
              }
            in
            records := r :: List.filter (fun r' -> r'.input != input) !records;
            r)

let id t = t.id
let elapsed t = t.elapsed
let advance t ~until = if until > t.elapsed then t.elapsed <- until
let close t = t.is_open <- false
let is_open t = t.is_open

let leave t =
  Mutex.protect lock (fun () ->
      t.users <- t.users - 1;
      if t.users <= 0 then records := List.filter (fun r -> r != t) !records)
