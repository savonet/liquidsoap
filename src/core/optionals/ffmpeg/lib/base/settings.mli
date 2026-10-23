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

(** The [settings.ffmpeg.*] keys (common.md §1). Each key's owner states when it
    is read. *)

val conf : Dtools.Conf.ut
val verbosities : string list
val log_verbosity : string Dtools.Conf.t
val log_level : int Dtools.Conf.t
val log_capture : bool Dtools.Conf.t
val autorotate : bool Dtools.Conf.t
val format_change_color : bool Dtools.Conf.t
val format_change_pixel_aspect : bool Dtools.Conf.t
val scaling_algorithms : string list

(** Rejects a value outside {!scaling_algorithms} when it is set. *)
val scaling_algorithm : string Dtools.Conf.t

val scaling_threads : int Dtools.Conf.t
val filter_max_buffer : float Dtools.Conf.t
val relaxed_compatibility_check : bool Dtools.Conf.t
val max_keyframe_wait : float Dtools.Conf.t
