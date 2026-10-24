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

let copy_name = "ffmpeg.copy"
let raw_audio_name = "ffmpeg.audio.raw"
let raw_video_name = "ffmpeg.video.raw"
let registered = ref None

let register name =
  match !registered with
    | Some other ->
        failwith
          (Printf.sprintf
             "FFmpeg content kinds are already registered with the %s \
              parameters; cannot register them again with the %s parameters."
             other name)
    | None -> registered := Some name

module Specs
    (K : Params.Kind)
    (N : sig
      type kind

      val kind : kind
      val kind_name : string
      val content_name : string
    end) =
struct
  type kind = N.kind
  type params = K.t

  let name = N.content_name
  let kind = N.kind
  let string_of_kind _ = N.kind_name
  let kind_of_string s = if s = N.kind_name then Some N.kind else None
  let merge = K.merge
  let compatible = K.compatible
  let string_of_params = K.to_string
  let parse_param = K.parse_param
  let serialize_params = K.serialize
  let parse_params = K.parse_serialized
  let default_params _ = K.default
  let content_lang_typ = Lang_core.string_t
  let params_to_value p = Lang_core.string (K.to_string p)
end

module Register (F : Content_base.Format) = struct
  include F

  let () =
    let kind = lift_kind kind in
    Type.register_type (Content_base.string_of_kind kind) (fun () ->
        Type.make (Type.Custom (Format_type.kind_handler (kind, Type.var ()))))
end

module Make (P : Params.S) = struct
  let () = register P.name

  module Copy =
    Register
      (Content_base.MkFormatBase
         (Specs
            (P.Copy)
            (struct
              type kind = [ `Copy ]

              let kind = `Copy
              let kind_name = copy_name
              let content_name = copy_name
            end)))

  module Audio =
    Register
      (Content_base.MkFormatBase
         (Specs
            (P.Audio)
            (struct
              type kind = [ `Raw_audio ]

              let kind = `Raw_audio
              let kind_name = raw_audio_name
              let content_name = "ffmpeg.raw.audio"
            end)))

  module Video =
    Register
      (Content_base.MkFormatBase
         (Specs
            (P.Video)
            (struct
              type kind = [ `Raw_video ]

              let kind = `Raw_video
              let kind_name = raw_video_name
              let content_name = "ffmpeg.raw.video"
            end)))
end
