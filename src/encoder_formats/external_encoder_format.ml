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

exception No_process

type restart_condition = Delay of int | Metadata | No_condition

type t = {
  channels : int;
  samplerate : int Lazy.t;
  video : bool;
  header : bool;
  restart_on_crash : bool;
  restart : restart_condition;
  process : string;
}

let to_string e =
  let string_of_restart_condition c =
    match c with
      | Delay d -> Printf.sprintf "restart_after_delay=%i" d
      | Metadata -> "restart_on_metadata"
      | No_condition -> ""
  in
  Printf.sprintf
    "%%external(channels=%i,samplerate=%i,video=%b,header=%b,restart_on_crash=%b,%s,process=%s)"
    e.channels (Lazy.force e.samplerate) e.video e.header e.restart_on_crash
    (string_of_restart_condition e.restart)
    e.process
