(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2017 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Lang_values
open Lang_encoders

let make ?pos params =
  let defaults =
    { Encoder.GStreamer.
       channels  = 2;
       audio     = Some "lamemp3enc";
       has_video = true;
       video     = Some "x264enc";
       muxer     = Some "mpegtsmux";
       metadata  = "metadata";
       log        = 5;
       pipeline  = None
    }
  in
  let gstreamer =
    let perhaps = function
      | "" -> None
      | s  -> Some s
    in
    List.fold_left
      (fun f ->
        function
          | ("channels",{ term = Int i; _}) ->
              { f with Encoder.GStreamer.channels = i }
          | ("audio",{ term = String s; _}) ->
              { f with Encoder.GStreamer.audio = perhaps s }
          | ("has_video",{ term = Bool b; _}) ->
              { f with Encoder.GStreamer.has_video = b }
          | ("video",{ term = String s; _}) ->
              { f with Encoder.GStreamer.video = perhaps s }
          | ("muxer",{ term = String s; _}) ->
              { f with Encoder.GStreamer.muxer = perhaps s }
          | ("metadata",{ term = String s; _}) ->
              { f with Encoder.GStreamer.metadata = s }
          | ("log",{ term = Int i; _}) ->
              { f with Encoder.GStreamer.log = i }
          | ("pipeline",{ term = String s; _}) ->
              { f with Encoder.GStreamer.pipeline = perhaps s }
          | (_,t) -> raise (generic_error t))
      defaults params
  in
  let dummy =
    { Lang_values.
        t = T.fresh_evar ~level:(-1) ~pos ;
        term = Encoder (Encoder.GStreamer gstreamer) }
  in
    if gstreamer.Encoder.GStreamer.pipeline = None &&
       gstreamer.Encoder.GStreamer.audio <> None &&
       gstreamer.Encoder.GStreamer.channels = 0
    then
      raise
        (Error (dummy, "must have at least one audio channel when \
                        passing an audio pipeline"));
    if gstreamer.Encoder.GStreamer.pipeline = None &&
       gstreamer.Encoder.GStreamer.video <> None &&
       gstreamer.Encoder.GStreamer.audio <> None &&
       gstreamer.Encoder.GStreamer.muxer = None
    then
      raise
        (Error (dummy, "must have a muxer when passing an audio and \
                        a video pipeline"));
    Encoder.GStreamer gstreamer
