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

(** [add_builtin ~base ~descr ~transport_t name proto make] registers an http
    transport whose server certificates can be read again. [make] receives the
    arguments and returns a function building the server config and one making
    the transport from a getter of the current config. The builtin gets a
    [reload_on] argument, once a day by default, and a [reload] method. *)
val add_builtin :
  base:Lang.module_name ->
  descr:string ->
  transport_t:Lang.t ->
  string ->
  Lang.proto ->
  ((string * Lang.value) list ->
  (unit -> 'config) * ((unit -> 'config) -> Lang.value)) ->
  unit
