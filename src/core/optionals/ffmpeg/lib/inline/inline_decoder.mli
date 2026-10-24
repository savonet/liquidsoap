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

(** The inline decoder transforms (inline-codecs.md §10, §11): [ffmpeg.copy] or
    raw content back to PCM and images, rebuilding a stage only on a new source
    id, time base or decoder parameters, after draining the old one. The track
    operators hand them to {!Producer.create}. *)

val audio : log:Log.t -> mode:[ `Copy | `Raw ] -> Producer.builder
val video : log:Log.t -> mode:[ `Copy | `Raw ] -> Producer.builder
