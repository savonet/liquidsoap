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

(** [input.ffmpeg] and [input.http] (io.md §1.1, §2–§9): a source reading one
    URL through libavformat, with the connection lifecycle of §2. The operators
    of {!Input_ffmpeg_operators} build it with {!create}; the start/stop layer
    starts and stops it; each connection attempt runs on a scheduler task,
    decoding runs on the streaming thread.

    Linking this module installs the shutdown flag of §2.4 that interrupts every
    blocking call of every connection. An [`Http] input sets its [icy] at each
    connection from the ICY headers received (§8). *)

type http = {
  user_agent : string;
  http_self_sync : unit -> bool option;
  mutable icy : bool;
}

type kind = [ `Ffmpeg of unit -> bool | `Http of http ]

type t =
  < Start_stop.active_source
  ; url : string
  ; set_url : (unit -> string) -> unit
  ; status : string
  ; register_on_connect : ((string * string) list -> unit) -> unit -> unit
  ; register_on_disconnect : (unit -> unit) -> unit -> unit
  ; register_on_error : (Runtime_error.runtime_error -> unit) -> unit -> unit >

val create :
  kind:kind ->
  url:(unit -> string) ->
  options:Avutil.opts ->
  format:(Avutil.input, Avutil.audio) Avutil.format option ->
  poll_delay:float ->
  timeout:float ->
  max_buffer:float ->
  metadata_filter:(Frame.metadata -> Frame.metadata) option ->
  deduplicate_metadata:bool ->
  new_track_on_metadata:bool ->
  debug:bool ->
  trim_url:bool ->
  autostart:bool ->
  unit ->
  t
