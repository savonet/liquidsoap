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

type copy_packet =
  [ `Audio of Avutil.audio Avcodec.Packet.t
  | `Video of Avutil.video Avcodec.Packet.t
  | `Subtitle of Avutil.subtitle Avcodec.Packet.t ]

let packet_media : copy_packet -> _ = function
  | `Audio _ -> `Audio
  | `Video _ -> `Video
  | `Subtitle _ -> `Subtitle

module Libav = struct
  let name = "libav"

  module Copy = struct
    type t = Copy_params.t option

    let default = None
    let to_string = function None -> "" | Some p -> Copy_params.to_string p
    let parse_param _ _ = None

    let compatible a b =
      match (a, b) with
        | Some a, Some b -> Copy_params.compatible a b
        | _ -> true

    let merge a b =
      match (a, b) with
        | Some a, Some b -> Some (Copy_params.merge a b)
        | None, p | p, None -> p

    let serialize _ = ""
    let parse_serialized = function "" -> Some None | _ -> None
  end

  module Audio = Raw_params.Audio
  module Video = Raw_params.Video
end

module Kinds = Liquidsoap_ffmpeg_formats.Kinds.Make (Libav)

module Packet_item = struct
  type t = copy_packet

  let copy : t -> t = function
    | `Audio p -> `Audio (Avcodec.Packet.dup p)
    | `Video p -> `Video (Avcodec.Packet.dup p)
    | `Subtitle p -> `Subtitle (Avcodec.Packet.dup p)

  let checksum : t -> string = function
    | `Audio p -> Digest.to_hex (Digest.string (Avcodec.Packet.content p))
    | `Video p -> Digest.to_hex (Digest.string (Avcodec.Packet.content p))
    | `Subtitle p -> Digest.to_hex (Digest.string (Avcodec.Packet.content p))
end

module Copy = struct
  module Data =
    Runs.Make
      (struct
        type t = Copy_params.t option

        let known = Option.is_some
      end)
      (Packet_item)

  include Content_base.MkDataBase (Kinds.Copy) (Data)

  let kind = lift_kind `Copy
end

module Audio = struct
  module Data =
    Runs.Make
      (struct
        include Raw_params.Audio

        let known p =
          not
            (Option.is_none p.channel_layout
            && Option.is_none p.sample_format
            && Option.is_none p.sample_rate)
      end)
      (struct
        type t = Avutil.audio Avutil.frame

        let copy = Fun.id
        let checksum = Raw_checksum.audio
      end)

  include Content_base.MkDataBase (Kinds.Audio) (Data)

  let kind = lift_kind `Raw_audio
end

module Video = struct
  module Data =
    Runs.Make
      (struct
        include Raw_params.Video

        let known p =
          not
            (Option.is_none p.width && Option.is_none p.height
            && Option.is_none p.pixel_format
            && Option.is_none p.pixel_aspect)
      end)
      (struct
        type t = Avutil.video Avutil.frame

        let copy = Fun.id
        let checksum = Raw_checksum.video
      end)

  include Content_base.MkDataBase (Kinds.Video) (Data)

  let kind = lift_kind `Raw_video
end
