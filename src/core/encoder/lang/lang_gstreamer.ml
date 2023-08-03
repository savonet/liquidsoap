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

open Value
open Ground

let log = Log.make ["gstreamer"]

let type_of_encoder p =
  let has_video =
    List.exists
      (function
        | "", `Term { Term.term = `Ground (Bool true) } -> true | _ -> false)
      p
  in
  let channels = Lang_encoder.channels_of_params p in
  let pcm_kind = Content.Audio.kind in
  if has_video then Encoder.audio_video_type ~pcm_kind channels
  else Encoder.audio_type ~pcm_kind channels

let make ?pos params =
  let () =
    log#important
      "Gstreamer is DEPRECATED! Please consider moving to FFMPEG. See: \
       https://github.com/savonet/liquidsoap/issues/2592 for some discussion \
       and example."
  in
  let defaults =
    {
      Gstreamer_format.channels = 2;
      audio = Some "lamemp3enc";
      has_video = true;
      video = Some "x264enc";
      muxer = Some "mpegtsmux";
      metadata = "metadata";
      log = 5;
      pipeline = None;
    }
  in
  let gstreamer =
    let perhaps = function "" -> None | s -> Some s in
    List.fold_left
      (fun f -> function
        | "stereo", `Value { value = Ground (Bool b); _ } ->
            { f with Gstreamer_format.channels = (if b then 2 else 1) }
        | "mono", `Value { value = Ground (Bool b); _ } ->
            { f with Gstreamer_format.channels = (if b then 1 else 2) }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "mono" ->
            { f with Gstreamer_format.channels = 1 }
        | "", `Value { value = Ground (String s); _ }
          when String.lowercase_ascii s = "stereo" ->
            { f with Gstreamer_format.channels = 2 }
        | "channels", `Value { value = Ground (Int i); _ } ->
            { f with Gstreamer_format.channels = i }
        | "audio", `Value { value = Ground (String s); _ } ->
            { f with Gstreamer_format.audio = perhaps s }
        | "has_video", `Value { value = Ground (Bool b); _ } ->
            { f with Gstreamer_format.has_video = b }
        | "video", `Value { value = Ground (String s); _ } ->
            let video = perhaps s in
            let has_video =
              if video = None then false else f.Gstreamer_format.has_video
            in
            { f with Gstreamer_format.has_video; video }
        | "muxer", `Value { value = Ground (String s); _ } ->
            { f with Gstreamer_format.muxer = perhaps s }
        | "metadata", `Value { value = Ground (String s); _ } ->
            { f with Gstreamer_format.metadata = s }
        | "log", `Value { value = Ground (Int i); _ } ->
            { f with Gstreamer_format.log = i }
        | "pipeline", `Value { value = Ground (String s); _ } ->
            { f with Gstreamer_format.pipeline = perhaps s }
        | t -> Lang_encoder.raise_generic_error t)
      defaults params
  in
  if
    gstreamer.Gstreamer_format.pipeline = None
    && gstreamer.Gstreamer_format.audio <> None
    && gstreamer.Gstreamer_format.channels = 0
  then
    Lang_encoder.raise_error ~pos
      "must have at least one audio channel when passing an audio pipeline";
  if
    gstreamer.Gstreamer_format.pipeline = None
    && gstreamer.Gstreamer_format.video <> None
    && gstreamer.Gstreamer_format.audio <> None
    && gstreamer.Gstreamer_format.muxer = None
  then
    Lang_encoder.raise_error ~pos
      "must have a muxer when passing an audio and a video pipeline";
  Encoder.GStreamer gstreamer

let () = Lang_encoder.register "gstreamer" type_of_encoder (make ?pos:None)
