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

type audio =
  | Speex of Speex_format.t
  | Vorbis of Vorbis_format.t
  | Flac of Flac_format.t
  | Opus of Opus_format.t

let string_of_audio = function
  | Vorbis v -> Vorbis_format.to_string v
  | Flac v -> Flac_format.to_string v
  | Speex s -> Speex_format.to_string s
  | Opus o -> Opus_format.to_string o

type video = Theora_format.t

let string_of_video = Theora_format.to_string

type t = { audio : audio option; video : video option }

let to_string { audio; video } =
  let l = match video with Some e -> [string_of_video e] | None -> [] in
  let l = match audio with Some e -> string_of_audio e :: l | None -> [] in
  Printf.sprintf "%%ogg(%s)" (String.concat "," l)
