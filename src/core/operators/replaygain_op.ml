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

class replaygain (source : source) =
  object (self)
    inherit operator ~name:"source.replaygain.compute" [source]
    val mutable override = None
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    val mutable state = None

    method reset =
      state <-
        Some
          (Audio.Analyze.ReplayGain.create ~channels:self#audio_channels
             ~samplerate:(Frame.audio_of_seconds 1.))

    method private state =
      if state = None then self#reset;
      Option.get state

    method private generate_frame =
      let buf = source#get_frame in
      Audio.Analyze.ReplayGain.process self#state (AFrame.pcm buf) 0
        source#frame_audio_position;
      buf

    method peak = Audio.Analyze.ReplayGain.peak self#state
    method gain = Audio.Analyze.ReplayGain.gain self#state
  end

let source_replaygain = Lang.add_module ~base:Muxer.source "replaygain"

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~audio:(Format_type.audio ()) ())
  in
  Lang.add_operator ~base:source_replaygain "compute"
    ~meth:
      Lang.
        [
          {
            name = "reset";
            scheme = ([], Lang.fun_t [] Lang.unit_t);
            descr = "Reset ReplayGain computation.";
            value =
              (fun s ->
                Lang.val_fun [] (fun _ ->
                    s#reset;
                    Lang.unit));
          };
          {
            name = "peak";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Peak sample.";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#peak));
          };
          {
            name = "gain";
            scheme = ([], Lang.fun_t [] Lang.float_t);
            descr = "Suggested gain (in dB).";
            value = (fun s -> Lang.val_fun [] (fun _ -> Lang.float s#gain));
          };
        ]
    [("", Lang.source_t frame_t, None, None)]
    ~return_t:frame_t ~category:`Audio
    ~descr:
      "Compute the ReplayGain of the source. Data is accumulated until the \
       `gain` method is called, i.e. the gain is computed _after_ the source \
       has been played.."
    (fun p ->
      let s = List.assoc "" p |> Lang.to_source in
      new replaygain s)
