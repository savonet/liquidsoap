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

(** The parameters of [ffmpeg.audio.raw] and [ffmpeg.video.raw] (content.md
    §3.2, §3.3), each property optional, merged per property. *)

module Audio : sig
  type t = {
    channel_layout : Avutil.Channel_layout.t option;
    sample_format : Avutil.Sample_format.t option;
    sample_rate : int option;
  }

  include Liquidsoap_ffmpeg_formats.Params.Kind with type t := t

  (** Every property set from the frame. *)
  val of_frame : Avutil.audio Avutil.frame -> t
end

module Video : sig
  type t = {
    width : int option;
    height : int option;
    pixel_format : Avutil.Pixel_format.t option;
    pixel_aspect : Avutil.rational option;
  }

  include Liquidsoap_ffmpeg_formats.Params.Kind with type t := t

  val of_frame : Avutil.video Avutil.frame -> t
end
