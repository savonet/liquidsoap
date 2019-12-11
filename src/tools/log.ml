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

(** Logging functions. *)

type t =
  < f: 'a. int -> ('a, unit, string, unit) format4 -> 'a
  ; critical: 'a. ('a, unit, string, unit) format4 -> 'a
  ; severe: 'a. ('a, unit, string, unit) format4 -> 'a
  ; important: 'a. ('a, unit, string, unit) format4 -> 'a
  ; info: 'a. ('a, unit, string, unit) format4 -> 'a
  ; debug: 'a. ('a, unit, string, unit) format4 -> 'a >

let make path : t =
  let log = Dtools.Log.make path in
  object
    (** Logging function. *)
    method f : 'a. int -> ('a, unit, string, unit) format4 -> 'a = log#f

    (** The program will not function after that. *)
    method critical : 'a. ('a, unit, string, unit) format4 -> 'a = log#f 1

    (** The behavior of the program will be strongly affected. *)
    method severe : 'a. ('a, unit, string, unit) format4 -> 'a = log#f 2

    (** The user should now about this. *)
    method important : 'a. ('a, unit, string, unit) format4 -> 'a = log#f 3

    (** The advanced user should be interested in this. *)
    method info : 'a. ('a, unit, string, unit) format4 -> 'a = log#f 4

    (** If you are debugging. *)
    method debug : 'a. ('a, unit, string, unit) format4 -> 'a = log#f 5
  end
