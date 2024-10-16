(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Ndi_format

let type_of_encoder p =
  match
    List.fold_left
      (fun (audio, video) -> function
        | `Encoder ("audio", []) -> (true, video)
        | `Encoder ("audio.none", []) -> (false, video)
        | `Encoder ("video", []) -> (audio, true)
        | `Encoder ("video.none", []) -> (audio, false)
        | _ -> (audio, video))
      (true, true) p
  with
    | true, true -> Encoder.audio_video_type ~pcm_kind:Content.Audio.kind 2
    | false, true -> Encoder.video_type ()
    | true, false -> Encoder.audio_type ~pcm_kind:Content.Audio.kind 2
    | _ -> Lang_encoder.raise_error ~pos:None "Invalid %%ndi encoder parameter!"

let make params =
  let defaults = { audio = true; video = true } in
  let ndi =
    List.fold_left
      (fun ndi -> function
        | `Encoder ("audio", []) -> { ndi with audio = true }
        | `Encoder ("audio.none", []) -> { ndi with audio = false }
        | `Encoder ("video", []) -> { ndi with video = true }
        | `Encoder ("video.none", []) -> { ndi with video = false }
        | `Labelled (_, v) ->
            Lang_encoder.raise_error ~pos:(Value.pos v) "Invalid parameter!"
        | _ ->
            Lang_encoder.raise_error ~pos:None
              "Invalid %%ndi encoder parameter!")
      defaults params
  in
  if (not ndi.audio) && not ndi.video then
    Lang_encoder.raise_error ~pos:None
      "%%ndi encoder needs at least one audio or video field!";
  Encoder.NDI ndi

let () = Lang_encoder.register "ndi" type_of_encoder make
