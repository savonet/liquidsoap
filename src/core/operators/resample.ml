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

class resample ~field ~ratio source =
  let source_val = Lang.source source in
  let write_frame_ref = ref (fun _ -> ()) in
  let consumer =
    new Producer_consumer.consumer
      ~write_frame:(fun _ frame -> !write_frame_ref frame)
      ~name:"stretch.consumer" ~source:source_val ()
  in
  let () = Typing.(consumer#frame_type <: source#frame_type) in
  object (self)
    inherit operator ~name:"stretch" []
    inherit Child_support.base ~check_self_sync:true [source_val]
    method self_sync = source#self_sync
    method fallible = source#fallible

    method! seek len =
      let glen = min (Generator.length self#buffer) len in
      Generator.truncate self#buffer glen;
      (if glen < len then source#seek (len - glen) else 0) + glen

    method effective_source = (self :> Source.source)

    method remaining =
      let rem = source#remaining in
      if rem = -1 then rem
      else int_of_float (float (rem + Generator.length self#buffer) *. ratio ())

    method abort_track = source#abort_track
    method private can_generate_frame = source#is_ready
    val mutable converter = None

    initializer
      self#on_wake_up (fun () ->
          converter <-
            Some (Audio_converter.Samplerate.create self#audio_channels);
          write_frame_ref := self#write_frame)

    method private write_frame =
      function `Frame frame -> self#process_frame frame | `Flush -> ()

    method private process_frame frame =
      let ratio = ratio () in
      let content = Content.Audio.get_data (Frame.get frame field) in
      let converter = Option.get converter in
      let pcm, offset, length =
        Audio_converter.Samplerate.resample converter ratio content 0
          (Audio.length content)
      in
      let offset = Frame_settings.main_of_audio offset in
      let length = Frame_settings.main_of_audio length in
      Generator.put self#buffer field
        (Content.Audio.lift_data ~offset ~length pcm);
      let convert x = int_of_float (float x *. ratio) in
      List.iter
        (fun (pos, m) ->
          Generator.add_metadata ~pos:(convert pos) self#buffer m)
        (Frame.get_all_metadata frame);
      List.iter
        (fun pos -> Generator.add_track_mark ~pos:(convert pos) self#buffer)
        (Frame.track_marks frame)

    method private generate_frame =
      consumer#set_output_enabled true;
      while
        Generator.length self#buffer < Lazy.force Frame.size && source#is_ready
      do
        self#child_tick
      done;
      consumer#set_output_enabled false;
      Generator.slice self#buffer (Lazy.force Frame.size)
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
