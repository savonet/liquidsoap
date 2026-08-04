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

let type_of_encoder p =
  let audio = ["vorbis"; "vorbis.cbr"; "vorbis.abr"; "opus"; "speex"; "flac"] in
  let audio =
    List.find_map
      (function
        | `Encoder (e, p) -> if List.mem e audio then Some p else None
        | _ -> None)
      p
  in
  let channels =
    match audio with None -> 0 | Some p -> Lang_encoder.channels_of_params p
  in
  let video =
    List.exists (function `Encoder ("theora", _) -> true | _ -> false) p
  in
  if not video then Encoder.audio_type ~pcm_kind:Content.Audio.kind channels
  else Encoder.audio_video_type ~pcm_kind:Content.Audio.kind channels

let make p =
  let ogg_audio e p =
    match e with
      | "vorbis" -> Lang_vorbis.make p
      | "vorbis.cbr" -> Lang_vorbis.make_cbr p
      | "vorbis.abr" -> Lang_vorbis.make_abr p
      | "opus" -> Lang_opus.make p
      | "speex" -> Lang_speex.make p
      | "flac" -> Lang_flac.make_ogg p
      | _ -> raise Not_found
  in
  let ogg_audio_opt e p = try Some (ogg_audio e p) with Not_found -> None in
  let ogg_video e p =
    match e with "theora" -> Lang_theora.make p | _ -> raise Not_found
  in
  let ogg_video_opt e p = try Some (ogg_video e p) with Not_found -> None in
  let audio =
    List.find_map
      (function `Encoder (e, p) -> ogg_audio_opt e p | _ -> None)
      p
  in
  let video =
    List.find_map
      (function `Encoder (e, p) -> ogg_video_opt e p | _ -> None)
      p
  in
  Encoder.Ogg { Ogg_format.audio; video }

let () = Lang_encoder.register "ogg" type_of_encoder make
