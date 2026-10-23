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

(** The three FFmpeg content kinds: their names, their type registration and
    their language form (content.md §1, §2).

    A process applies {!Make} once, to the parameter implementation it links. A
    second application raises, naming both implementations. *)

val copy_name : string
val raw_audio_name : string
val raw_video_name : string

module Make (P : Params.S) : sig
  module Copy :
    Content_base.Format with type kind = [ `Copy ] and type params = P.Copy.t

  module Audio :
    Content_base.Format
      with type kind = [ `Raw_audio ]
       and type params = P.Audio.t

  module Video :
    Content_base.Format
      with type kind = [ `Raw_video ]
       and type params = P.Video.t
end
