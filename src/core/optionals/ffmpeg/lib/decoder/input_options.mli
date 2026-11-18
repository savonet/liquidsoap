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

(** Demuxer options of an input (decoder.md §4): the [ffmpeg_options] request
    metadata, the parameters of an [application/ffmpeg] MIME type, the image
    options, and the check that the open consumed them all. Every error is of
    kind [ffmpeg_decoder]. *)

type input_format = (Avutil.input, Avutil.audio) Avutil.format

(** Decoder options per media type (decoder.md §4.4), keyed without their
    [audio.], [video.] or [subtitle.] prefix. *)
type decoder_options = {
  audio : Avutil.opts;
  video : Avutil.opts;
  subtitle : Avutil.opts;
}

type t = {
  format : input_format option;
  opts : Avutil.opts;
  decoder : decoder_options;
}

val none : unit -> t
val no_decoder_options : unit -> decoder_options

(** The key prefixes of decoder options: [audio.], [video.], [subtitle.]. *)
val decoder_prefixes : string list

(** Moves the prefixed keys of [opts] into the decoder options it returns,
    leaving the demuxer options in [opts].

    @raise the [ffmpeg_decoder] error for a prefix with no option name. *)
val split_decoder_options : Avutil.opts -> decoder_options

(** Parses [%ffmpeg]-style arguments: [f=]/[format=] forces the demuxer, a key
    prefixed by a media type is a decoder option, any other [key=value] is a
    demuxer option. *)
val parse : what:string -> string -> t

(** From the request metadata [ffmpeg_options]. *)
val of_metadata : Frame.metadata -> t

(** From a MIME type [application/ffmpeg;…]. *)
val of_mime : string -> t

(** Sets [loop = 1] and [framerate = <video rate>]. *)
val add_image_options : t -> unit

(** @raise the [ffmpeg_decoder] error listing what the open left. *)
val check_unused : t -> unit

val error : string -> 'a
