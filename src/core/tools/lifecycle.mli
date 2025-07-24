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

(** Module to expose and hook up actions that need to happen over the lifecycle
    of the application. *)

(** Initial load. Used for registering stuff that needs all OCaml modules to
    have been registered. *)
val on_load : name:string -> (unit -> unit) -> unit

val load : unit -> unit

(** {1 Initialization} *)

(** Initialization proceeds in 3 phases:
    - Init
    - Script parse
    - Application start *)

(** {2 Init start} *)

val init : unit -> unit
val before_init : name:string -> (unit -> unit) -> unit
val on_init : name:string -> (unit -> unit) -> unit
val after_init : name:string -> (unit -> unit) -> unit

(** {2 Script parse} *)

val before_script_parse : name:string -> (unit -> unit) -> unit
val on_script_parse : name:string -> (unit -> unit) -> unit
val after_script_parse : name:string -> (unit -> unit) -> unit

(** {2 Application start} *)

val before_start : name:string -> (unit -> unit) -> unit
val on_start : name:string -> (unit -> unit) -> unit
val after_start : name:string -> (unit -> unit) -> unit

(** {2 Application main loop} *)
val before_main_loop : name:string -> (unit -> unit) -> unit

val on_main_loop : name:string -> (unit -> unit) -> unit
val after_main_loop : name:string -> (unit -> unit) -> unit

(** {1 Shutdown} *)

(** Shutdown proceeds in 3 phases:
    - Core shutdown
    - Scheduler shutdown
    - Final cleanup
    - Stop *)

(** {2 Core shutdown} *)

val before_core_shutdown : name:string -> (unit -> unit) -> unit
val on_core_shutdown : name:string -> (unit -> unit) -> unit
val after_core_shutdown : name:string -> (unit -> unit) -> unit

(** {2 Scheduler shutdown} *)

val before_scheduler_shutdown : name:string -> (unit -> unit) -> unit
val on_scheduler_shutdown : name:string -> (unit -> unit) -> unit
val after_scheduler_shutdown : name:string -> (unit -> unit) -> unit

(** {2 Final cleanup} *)

val before_final_cleanup : name:string -> (unit -> unit) -> unit
val on_final_cleanup : name:string -> (unit -> unit) -> unit
val after_final_cleanup : name:string -> (unit -> unit) -> unit
