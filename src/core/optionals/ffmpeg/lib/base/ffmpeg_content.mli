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

(** The FFmpeg content kinds with libav parameters and run-layer data
    (content.md). Linking this module registers the kinds. *)

type copy_packet =
  [ `Audio of Avutil.audio Avcodec.Packet.t
  | `Video of Avutil.video Avcodec.Packet.t
  | `Subtitle of Avutil.subtitle Avcodec.Packet.t ]

module Copy : sig
  module Data :
    Runs.S with type item = copy_packet and type params = Copy_params.t option

  include
    Content_base.Content
      with type kind = [ `Copy ]
       and type params = Copy_params.t option
       and type data = Data.data

  val kind : Content_base.kind
end

module Audio : sig
  module Data :
    Runs.S
      with type item = Avutil.audio Avutil.frame
       and type params = Raw_params.Audio.t

  include
    Content_base.Content
      with type kind = [ `Raw_audio ]
       and type params = Raw_params.Audio.t
       and type data = Data.data

  val kind : Content_base.kind
end

module Video : sig
  module Data :
    Runs.S
      with type item = Avutil.video Avutil.frame
       and type params = Raw_params.Video.t

  include
    Content_base.Content
      with type kind = [ `Raw_video ]
       and type params = Raw_params.Video.t
       and type data = Data.data

  val kind : Content_base.kind
end

val packet_media : copy_packet -> [ `Audio | `Video | `Subtitle ]
