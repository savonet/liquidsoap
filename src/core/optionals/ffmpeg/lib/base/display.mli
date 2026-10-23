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

(** The display stage of side-data.md §3: decoded video frames delivered upright
    and cropped by their display matrix, unless [settings.ffmpeg.autorotate] is
    off when the stage is created.

    [on_undecided] holds the script's handler of side-data.md §4, when one is
    installed; stages read it when they are created. *)

type t

val on_undecided :
  (Avfilter.Utils.undecided_frame -> Avfilter.Utils.filter_spec list option)
  option
  ref

val create :
  ?params:Avutil.video Avcodec.params -> time_base:Avutil.rational -> unit -> t

val convert :
  t -> (Avutil.video Avutil.frame -> unit) -> Avutil.video Avutil.frame -> unit

(** Delivers the frames the stage holds; the stage stays usable. *)
val eof : t -> (Avutil.video Avutil.frame -> unit) -> unit

(** Width and height of the displayed layout of a stream (§3.3). *)
val expected_size : Avutil.video Avcodec.params -> int * int

(** Pixel aspect of the displayed layout (§3.3). *)
val pixel_aspect :
  ?stored:Avutil.rational ->
  Avutil.video Avcodec.params ->
  Avutil.rational option
