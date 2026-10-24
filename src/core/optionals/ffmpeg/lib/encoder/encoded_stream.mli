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

(** The stream encoders of internal, raw and subtitle tracks
    (encoder-internal.md), one per encoded track of a container. The instance
    calls {!create} at the container's build, then [mk_stream] with the
    container's first frame in track order, [encode] for each frame of positive
    length, and [flush] before the container closes.

    [create] raises the [encoder] error of a configuration it cannot build;
    [mk_stream] raises it for a stream the codec or the muxer rejects. [encode]
    raises [Failure] once a field reaches the fault bound. The [on_packet] hook
    is called before each packet reaches the muxer, with its key flag and its
    media time in main ticks (encoder.md §8.2). *)

type on_packet = key:bool -> time:int option -> unit

(** [request_keyframe time] makes the first image at or after media time [time]
    a key frame; a no-op on audio and subtitle streams (encoder.md §8.4). *)
type t = {
  mk_stream : Frame.t -> unit;
  encode : position:int -> Frame.t -> unit;
  flush : unit -> unit;
  request_keyframe : int -> unit;
  codec_attr : unit -> string option;
  bitrate : unit -> int option;
  video_size : unit -> (int * int) option;
}

val audio_codec_attr : (_, Avutil.audio, _) Av.stream -> string option

val create :
  log:Log.t ->
  pos:Pos.t option ->
  field:Frame.field ->
  on_packet:on_packet ->
  Liquidsoap_ffmpeg_formats.Encoder_format.encoded ->
  Avutil.output Avutil.container ->
  t
