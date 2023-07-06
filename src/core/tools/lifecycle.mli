(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Module to expose and hook up actions that need to happen
    over the lifecycle of the application. *)

(** {1 Initialization} *)

(** Initialization proceeds in 3 phases:
    - Init
    - Script parse
    - Application start *)

(** {2 Init start} *)

val init_atom : Dtools.Init.t
val before_init : (unit -> unit) -> unit
val on_init : (unit -> unit) -> unit
val after_init : (unit -> unit) -> unit

(** {2 Script parse} *)

val before_script_parse : (unit -> unit) -> unit
val on_script_parse : (unit -> unit) -> unit
val after_script_parse : (unit -> unit) -> unit

(** {2 Application start} *)

val before_start : (unit -> unit) -> unit
val on_start : (unit -> unit) -> unit
val after_start : (unit -> unit) -> unit

(** {2 Set application main loop} *)
val main_loop : (unit -> unit) -> unit

(** {1 Shutdown} *)

(** Shutdown proceeds in 3 phases:
    - Core shutdown
    - Scheduler shutdown
    - Final cleanup
    - Stop *)

(** {2 Core shutdown} *)

val before_core_shutdown : (unit -> unit) -> unit
val on_core_shutdown : (unit -> unit) -> unit
val after_core_shutdown : (unit -> unit) -> unit

(** {2 Scheduler shutdown} *)

val before_scheduler_shutdown : (unit -> unit) -> unit
val on_scheduler_shutdown : (unit -> unit) -> unit
val after_scheduler_shutdown : (unit -> unit) -> unit

(** {2 Final cleanup} *)

val before_final_cleanup : (unit -> unit) -> unit
val on_final_cleanup : (unit -> unit) -> unit
val after_final_cleanup : (unit -> unit) -> unit

(** {2 Stop} *)

val before_stop : (unit -> unit) -> unit
val on_stop : (unit -> unit) -> unit
val after_stop : (unit -> unit) -> unit
