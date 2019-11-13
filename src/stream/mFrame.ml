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

open Frame

type t = Frame.t

let mot = midi_of_master

let tom = master_of_midi

let size () = mot (Lazy.force Frame.size)

let position t = mot (position t)

let content b pos =
  let stop, content = content b (tom pos) in
  assert (stop = size ()) ;
  content.midi

let content_of_type ~channels b pos =
  let ctype = {audio= 0; video= 0; midi= channels} in
  let content = content_of_type b (tom pos) ctype in
  content.midi

let add_break t i = add_break t (tom i)

let is_partial = is_partial

type metadata = (string, string) Hashtbl.t

let set_metadata t i m = set_metadata t (tom i) m

let get_metadata t i = get_metadata t (tom i)

let get_all_metadata t =
  List.map (fun (x, y) -> (mot x, y)) (get_all_metadata t)
