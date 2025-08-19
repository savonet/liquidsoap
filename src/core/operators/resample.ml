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

class resample ~field ~ratio source =
  object (self)
    inherit
      Child_support.producer
        ~check_self_sync:true ~name:"stretch" (Lang.source source)

    val mutable converter = None

    initializer
      self#child#set_process_frame self#process_frame;
      self#on_wake_up (fun () ->
          converter <-
            Some (Audio_converter.Samplerate.create self#audio_channels))

    method private process_frame gen frame =
      let ratio = ratio () in
      let content = Content.Audio.get_data (Frame.get frame field) in
      let converter = Option.get converter in
      let pcm, offset, length =
        Audio_converter.Samplerate.resample converter ratio content 0
          (Audio.length content)
      in
      let offset = Frame_settings.main_of_audio offset in
      let length = Frame_settings.main_of_audio length in
      Generator.put gen field (Content.Audio.lift_data ~offset ~length pcm);
      let convert x = int_of_float (float x *. ratio) in
      List.iter
        (fun (pos, m) -> Generator.add_metadata ~pos:(convert pos) gen m)
        (Frame.get_all_metadata frame);
      List.iter
        (fun pos -> Generator.add_track_mark ~pos:(convert pos) gen)
        (Frame.track_marks frame)
  end

let _ =
  let return_t = Format_type.audio () in
  Lang.add_track_operator ~base:Modules.track_audio
    "stretch" (* TODO better name *)
    [
      ( "ratio",
        Lang.getter_t Lang.float_t,
        None,
        Some "A value higher than 1 means slowing down." );
      ("", return_t, None, None);
    ]
    ~return_t ~category:`Audio
    ~descr:
      "Slow down or accelerate an audio stream by stretching (sounds lower) or \
       squeezing it (sounds higher)."
    (fun p ->
      let f v = List.assoc v p in
      let field, src = Lang.to_track (f "") in
      let ratio = Lang.to_float_getter (f "ratio") in
      (field, (new resample ~field ~ratio src :> Source.source)))
