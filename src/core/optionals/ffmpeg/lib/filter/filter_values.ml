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

module Graph_value = Value.MkCustom (struct
  type content = Graph.t

  let name = "ffmpeg.filter.graph"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error ~pos
      ~message:"Ffmpeg filter graph cannot be represented as json" "json"

  let compare (a : content) (b : content) =
    compare
      (Description.gid (Graph.description a))
      (Description.gid (Graph.description b))
end)

module Audio_pad = Value.MkCustom (struct
  type content = [ `Audio ] Description.pad

  let name = "ffmpeg.filter.audio"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error ~pos
      ~message:"Ffmpeg filter audio input cannot be represented as json" "json"

  let compare (a : content) (b : content) = compare a.pid b.pid
end)

module Video_pad = Value.MkCustom (struct
  type content = [ `Video ] Description.pad

  let name = "ffmpeg.filter.video"
  let to_string _ = name

  let to_json ~pos _ =
    Lang.raise_error ~pos
      ~message:"Ffmpeg filter video input cannot be represented as json" "json"

  let compare (a : content) (b : content) = compare a.pid b.pid
end)
