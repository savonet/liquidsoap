(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

include Content_base

module MkContent (C : ContentSpecs) = struct
  include MkContentBase (C)

  let () =
    Type.register_type (C.string_of_kind C.kind) (fun () ->
        Type.make
          (Type.Custom
             (Format_type.kind_handler
                (lift_kind C.kind, Liquidsoap_lang.Lang.univ_t ()))))
end

include Content_timed

type audio_params = Content_audio.Specs.params = {
  channel_layout : [ `Mono | `Stereo | `Five_point_one ] SyncLazy.t;
}

type video_params = Content_video.Specs.params = {
  width : int SyncLazy.t option;
  height : int SyncLazy.t option;
}

type midi_params = Content_midi.Specs.params = { channels : int }

module Audio = Content_audio
module Video = Content_video
module Midi = Content_midi
