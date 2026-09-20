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

(** What a content type is, and how it is described to the language. Depends on
    the language alone: no frames, no mm buffers, no settings, so that a process
    which only typechecks scripts can link it. *)

module Audio_format = Audio_format
module Audio_layout = Audio_layout
module Content_base = Content_base
module Fields = Fields
module Format_type = Format_type
module Frame_type = Frame_type
module Midi_format = Midi_format
module Pcm_format = Pcm_format
module Subtitle_format = Subtitle_format
module Timed_format = Timed_format
module Unifier = Unifier
module Video_format = Video_format
