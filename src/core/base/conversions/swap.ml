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

open Source

class swap ~field (source : source) =
  object
    inherit operator [source] ~name:"swap"

    inherit
      Conversion.base
        ~converter:(fun frame ->
          let buffer = Content.Audio.get_data (Frame.get frame field) in
          Frame.set_data frame field Content.Audio.lift_data
            [| buffer.(1); buffer.(0) |])
        source
  end

let _ =
  let track_t = Format_type.audio_stereo () in
  Lang.add_track_operator ~base:Modules.track_audio "swap"
    [("", track_t, None, None)]
    ~return_t:track_t ~category:`Conversion
    ~descr:"Swap two channels of a stereo track."
    (fun p ->
      let field, s = Lang.to_track (Lang.assoc "" 1 p) in
      (field, new swap ~field s))
