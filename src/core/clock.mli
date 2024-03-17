(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

exception Invalid_state

type t
type active_source = < reset : unit ; output : unit >

type source_type =
  [ `Passive | `Active of active_source | `Output of active_source ]

type sync_source
type self_sync = [ `Static | `Dynamic ] * sync_source option

val string_of_sync_source : sync_source -> string

module type SyncSource = sig
  type t

  val to_string : t -> string
end

module MkSyncSource (S : SyncSource) : sig
  val make : S.t -> sync_source
end

type source =
  < id : string
  ; self_sync : self_sync
  ; source_type : source_type
  ; active : bool
  ; wake_up : unit
  ; sleep : unit
  ; is_ready : bool
  ; get_frame : Frame.t >

type active_sync_mode = [ `Automatic | `CPU | `Unsynced | `Passive ]
type sync_mode = [ active_sync_mode | `Stopping | `Stopped ]

val string_of_sync_mode : sync_mode -> string
val active_sync_mode_of_string : string -> active_sync_mode

val create :
  ?pos:Liquidsoap_lang.Pos.t ->
  ?on_error:(exn -> Printexc.raw_backtrace -> unit) ->
  ?id:string ->
  ?sync:active_sync_mode ->
  unit ->
  t

val main : t
val clocks : unit -> t list
val id : t -> string
val descr : t -> string
val sync : t -> sync_mode
val start : ?force:bool -> t -> unit
val stop : t -> unit
val set_pos : t -> Liquidsoap_lang.Pos.Option.t -> unit
val self_sync : t -> bool
val unify : pos:Liquidsoap_lang.Pos.Option.t -> t -> t -> unit
val create_sub_clock : id:string -> t -> t
val attach : t -> source -> unit
val detach : t -> source -> unit
val ticks : t -> int
val on_tick : t -> (unit -> unit) -> unit
val tick : t -> unit
val after_tick : t -> (unit -> unit) -> unit
val time_implementation : unit -> Liq_time.implementation
