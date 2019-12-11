(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type item =
  | Speex of Speex_format.t
  | Vorbis of Vorbis_format.t
  | Flac of Flac_format.t
  | Theora of Theora_format.t
  | Opus of Opus_format.t

type t = item list

let to_string l =
  Printf.sprintf "%%ogg(%s)"
    (String.concat ","
       (List.map
          (function
            | Vorbis v ->
                Vorbis_format.to_string v
            | Flac v ->
                Flac_format.to_string v
            | Theora t ->
                Theora_format.to_string t
            | Speex s ->
                Speex_format.to_string s
            | Opus o ->
                Opus_format.to_string o)
          l))
