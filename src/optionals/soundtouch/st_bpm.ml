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

class bpm (source : source) =
  object (self)
    inherit operator ~name:"bpm" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method self_sync = source#self_sync
    method remaining = source#remaining
    method effective_source = source#effective_source
    method abort_track = source#abort_track
    val mutable bpm = None

    initializer
      self#on_wake_up (fun () ->
          bpm <-
            Some
              (Soundtouch.BPM.make
                 (Content.Audio.channels_of_format
                    (Option.get
                       (Frame.Fields.find_opt Frame.Fields.audio
                          self#content_type)))
                 (Lazy.force Frame.audio_rate)))

    method private generate_frame =
      let buf =
        Content.Audio.get_data (source#get_mutable_content Frame.Fields.audio)
      in
      let bpm = Option.get bpm in
      let len = source#frame_audio_position in
      Soundtouch.BPM.put_samples_ni bpm buf 0 len;
      source#set_frame_data Frame.Fields.audio Content.Audio.lift_data buf

    method bpm =
      match bpm with Some bpm -> Soundtouch.BPM.get_bpm bpm | None -> 0.
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator "bpm"
    [("", Lang.source_t frame_t, None, None)]
    ~return_t:frame_t ~category:`Visualization
    ~descr:
      "Detect the BPM (number of beats per minute). The returned source has a \
       method `bpm`, which can be called to compute it."
    ~meth:
      [
        {
          name = "bpm";
          scheme = ([], Lang.fun_t [] Lang.float_t);
          descr = "Compute the current BPM.";
          value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#bpm));
        };
      ]
    (fun p ->
      let s = Lang.to_source (List.assoc "" p) in
      new bpm s)
