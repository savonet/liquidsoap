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

type header = Liquidsoap_ffmpeg_formats.Copy_header.t
type copy_format = { params : Copy_params.t option; header : header }

module Libav = struct
  let name = "libav"

  module Copy = struct
    module H = Liquidsoap_ffmpeg_formats.Copy_header

    type t = copy_format

    let default = { params = None; header = H.default }

    let to_string { params; header } =
      String.concat ","
        (List.filter
           (fun s -> s <> "")
           [
             Option.fold ~none:"" ~some:Copy_params.to_string params;
             H.to_string header;
           ])

    let parse_param label value =
      Option.map
        (fun header -> { params = None; header })
        (H.parse_param label value)

    (* content.md §3.1.1: header details are compared unless every output of
       the track carries them in band. *)
    let incompatibility a b =
      match (a.params, b.params) with
        | Some p, Some p' ->
            Copy_params.incompatibility
              ~headers:(H.headers (H.merge a.header b.header))
              p p'
        | _ -> None

    let compatible a b = incompatibility a b = None

    let merge a b =
      (match incompatibility a b with
        | Some reason ->
            failwith
              (Printf.sprintf "Incompatible copy parameters: %s: %s and %s"
                 reason (to_string a) (to_string b))
        | None -> ());
      let header = H.merge a.header b.header in
      {
        params =
          (match (a.params, b.params) with
            | Some p, Some p' ->
                Some (Copy_params.merge ~headers:(H.headers header) p p')
            | None, p | p, None -> p);
        header;
      }

    let serialize { header } = H.to_string header

    let parse_serialized s =
      Option.map
        (fun header -> { params = None; header })
        (H.parse_serialized s)
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
        type t = copy_format

        let known { params } = Option.is_some params
      end)
      (Packet_item)

  include Content_base.MkDataBase (Kinds.Copy) (Data)

  let kind = lift_kind `Copy

  let unconstrained params =
    { params; header = Liquidsoap_ffmpeg_formats.Copy_header.default }

  let of_params params = lift_params (unconstrained (Some params))
  let stream_params format = (get_params format).params

  let incompatibility format params =
    Libav.Copy.incompatibility (get_params format) (unconstrained (Some params))
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
