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

open Mm
open Frame

type t = Frame.t

let sot = audio_of_main
let content b = try Frame.audio b with Not_found -> raise Content.Invalid
let pcm b = Content.Audio.get_data (content b)
let duration () = Lazy.force duration
let size () = sot (Lazy.force size)
let position t = sot (position t)
let rms b off len = Audio.Analyze.rms (pcm b) off len

let s16le b =
  let pcm = Content.Audio.get_data (content b) in
  let channels = Array.length pcm in
  let len = position b in
  let buf = Bytes.create (Audio.S16LE.size channels len) in
  Audio.S16LE.of_audio pcm 0 buf 0 len;
  Bytes.unsafe_to_string buf
