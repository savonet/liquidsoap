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

open Content_base

(* The pcm kinds differ by the buffers they hold, which is none of the
   language's business: their parameters are the audio ones. *)
module Shared = struct
  type params = Audio_format.Specs.params

  let merge = Audio_format.Specs.merge
  let compatible = Audio_format.Specs.compatible
  let string_of_params = Audio_format.Specs.string_of_params
  let parse_param = Audio_format.Specs.parse_param
  let serialize_params = Audio_format.Specs.serialize_params
  let parse_params = Audio_format.Specs.parse_params
  let param_of_channels = Audio_format.Specs.param_of_channels
  let content_lang_typ = Audio_format.Specs.content_lang_typ
  let params_to_value = Audio_format.Specs.params_to_value
  let default_params _ = param_of_channels (!Audio_format.default_channels ())
end

module S16 = struct
  module Specs = struct
    include Shared

    type kind = [ `Pcm_s16 ]

    let name = "pcm_s16"
    let kind = `Pcm_s16
    let string_of_kind = function `Pcm_s16 -> "pcm_s16"
    let kind_of_string = function "pcm_s16" -> Some `Pcm_s16 | _ -> None
  end

  module Format = MkFormatBase (Specs)
  include Format

  let kind = lift_kind `Pcm_s16
end

module F32 = struct
  module Specs = struct
    include Shared

    type kind = [ `Pcm_f32 ]

    let name = "pcm_f32"
    let kind = `Pcm_f32
    let string_of_kind = function `Pcm_f32 -> "pcm_f32"
    let kind_of_string = function "pcm_f32" -> Some `Pcm_f32 | _ -> None
  end

  module Format = MkFormatBase (Specs)
  include Format

  let kind = lift_kind `Pcm_f32
end

let channels_of_format ~get_params p =
  Audio_format.Specs.(
    channels_of_param (Lazy.Mutexed.force (get_params p).channel_layout))

(* Which pcm content a format belongs to is a kind, and nothing else. *)
let audio_format ~pcm_kind params =
  let lift_params =
    match pcm_kind with
      | _ when Audio_format.is_kind pcm_kind -> Audio_format.lift_params
      | _ when S16.is_kind pcm_kind -> S16.lift_params
      | _ when F32.is_kind pcm_kind -> F32.lift_params
      | _ -> raise Invalid
  in
  lift_params params

let format_of_channels ~pcm_kind n =
  audio_format ~pcm_kind
    {
      Audio_format.Specs.channel_layout =
        Lazy.Mutexed.from_val (Audio_layout.layout_of_channels n);
    }
