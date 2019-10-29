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

let log = Log.make ["lang";"commad"]

let () =
  let t = Lang.univ_t 1 in
  let cmd_t = Lang_values.cmd_t ~pos:t.Lang_types.pos t in
  Lang_builtins.add_builtin
    ~cat:Lang_builtins.Control
    ~descr:"Set the value of a command."
    "command.set"
    ["", cmd_t, None, None;
     "", t, None, None ] Lang.unit_t
    (fun p ->
       let c = Lang.assoc "" 1 p in
       let v = Lang.assoc "" 2 p in
       (
         match c.Lang.value with
         | Lang.Cmd c ->
           (* This can should never happen since non-None value get changed into
              the value itself. *)
           assert (!c = None);
           c := Some v
         | _ -> failwith "Trying to use and undefined command."
       );
       Lang.unit
    )
