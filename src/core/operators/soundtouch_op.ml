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

class soundtouch source_val rate tempo pitch =
  let source = Lang.to_source source_val in
  let write_frame_ref = ref (fun _ -> ()) in
  let consumer =
    new Producer_consumer.consumer
      ~write_frame:(fun _ frame -> !write_frame_ref frame)
      ~name:"soundtouch.consumer" ~source:source_val ()
  in
  let () =
    Typing.(consumer#frame_type <: source#frame_type);
    Typing.(source#frame_type <: consumer#frame_type)
  in
  object (self)
    inherit operator ~name:"soundtouch" []
    inherit Child_support.base ~check_self_sync:true [source_val]
    val mutable st = None
    method fallible = source#fallible
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method effective_source = source#effective_source
    method remaining = -1

    method abort_track =
      Generator.add_track_mark self#buffer;
      source#abort_track

    method private write_frame =
      function `Frame databuf -> self#process_frame databuf | `Flush -> ()

    method process_frame databuf =
      let st = Option.get st in
      Soundtouch.set_rate st (rate ());
      Soundtouch.set_tempo st (tempo ());
      Soundtouch.set_pitch st (pitch ());
      let db = AFrame.pcm databuf in
      Soundtouch.put_samples_ni st db 0 (AFrame.position databuf);
      let available = Soundtouch.get_available_samples st in
      if available > 0 then (
        let buf = Audio.create self#audio_channels available in
        ignore (Soundtouch.get_samples_ni st buf 0 available);
        Generator.put self#buffer Frame.Fields.audio
          (Content.Audio.lift_data buf));
      let gen_pos = Generator.length self#buffer in
      List.iter
        (fun pos -> Generator.add_track_mark ~pos:(pos + gen_pos) self#buffer)
        (Frame.track_marks databuf);

      List.iter
        (fun (pos, m) ->
          Generator.add_metadata ~pos:(pos + gen_pos) self#buffer m)
        (Frame.get_all_metadata databuf)

    method private generate_frame =
      let size = Lazy.force Frame.size in
      consumer#set_output_enabled true;
      while Generator.length self#buffer < size && source#is_ready do
        self#child_tick
      done;
      consumer#set_output_enabled false;
      Generator.slice self#buffer size

    initializer
      self#on_wake_up (fun () ->
          st <-
            Some
              (Soundtouch.make self#audio_channels
                 (Lazy.force Frame.audio_rate));
          self#log#important "Using soundtouch %s."
            (Soundtouch.get_version_string (Option.get st));
          write_frame_ref := self#write_frame)
  end

let _ =
  (* TODO: could we keep the video in some cases? *)
  let return_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "soundtouch"
    [
      ("rate", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("tempo", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("pitch", Lang.getter_t Lang.float_t, Some (Lang.float 1.0), None);
      ("", Lang.source_t return_t, None, None);
    ]
    ~category:`Audio ~return_t
    ~descr:"Change the rate, the tempo or the pitch of the sound."
    ~flags:[`Experimental]
    (fun p ->
      let f v = List.assoc v p in
      let rate = Lang.to_float_getter (f "rate") in
      let tempo = Lang.to_float_getter (f "tempo") in
      let pitch = Lang.to_float_getter (f "pitch") in
      let s = f "" in
      (new soundtouch s rate tempo pitch :> Source.source))
