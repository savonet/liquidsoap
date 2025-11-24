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

open Mm
open Source

class clip ~field (source : source) =
  object
    inherit operator ~name:"clip" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method self_sync = source#self_sync

    method private generate_frame =
      let c = source#get_mutable_content field in
      let b = Content.Audio.get_data c in
      let position = source#frame_audio_position in
      Audio.clip b 0 position;
      source#set_frame_data field Content.Audio.lift_data b
  end

let _ =
  let frame_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio "clip"
    [("", frame_t, None, None)]
    ~return_t:frame_t ~category:`Audio
    ~descr:
      "Clip samples, i.e. ensure that all values are between -1 and 1: values \
       lower than -1 become -1 and values higher than 1 become 1. `nan` values \
       become `0.`"
    (fun p ->
      let f v = List.assoc v p in
      let field, src = Lang.to_track (f "") in
      (field, new clip ~field src))
