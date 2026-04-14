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

let format = Lang.add_module "format"

let _ =
  Lang.add_builtin ~base:format "description" ~category:(`Source `Liquidsoap)
    ~descr:
      "Return a description of the given format as a record with optional \
       methods. For PCM audio: `{ channels, channel_layout }`. For YUV420P \
       video: `{ width, height }`. For MIDI: `{ channels }`. Returns an empty \
       record for formats without a description (e.g. metadata, track marks)."
    [("", Content.Format_val.t, None, None)]
    (Content.content_types ())
    (fun p ->
      let fmt = Content.Format_val.of_value (List.assoc "" p) in
      match Content.value_of_format fmt with
        | Some (name, v) -> Lang.meth (Lang.record []) [(name, v)]
        | None -> Lang.record [])
