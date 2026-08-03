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

open Value

let make params =
  let video_width, video_height = Frame.video_dimensions () in
  let defaults =
    {
      (* We use a hardcoded value in order not to force the evaluation of the
         number of channels too early, see #933. *)
      Avi_format.channels = 2;
      samplerate = Frame.audio_rate;
      width = video_width;
      height = video_height;
    }
  in
  let avi =
    List.fold_left
      (fun f -> function
        | `Labelled ("channels", Int { value = c }) ->
            { f with Avi_format.channels = c }
        | `Labelled ("samplerate", Int { value = i; _ }) ->
            { f with Avi_format.samplerate = Lazy.from_val i }
        | `Labelled ("width", Int { value = i; _ }) ->
            { f with Avi_format.width = Lazy.from_val i }
        | `Labelled ("height", Int { value = i; _ }) ->
            { f with Avi_format.height = Lazy.from_val i }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  Encoder.AVI avi

let type_of_encoder p =
  Encoder.audio_video_type ~pcm_kind:Content.Audio.kind
    (Lang_encoder.channels_of_params p)

let () = Lang_encoder.register "avi" type_of_encoder make
