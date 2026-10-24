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
type t = { format : input_format option; opts : Avutil.opts }

val none : unit -> t

(** Parses [%ffmpeg]-style arguments: [f=]/[format=] forces the demuxer, any
    other [key=value] is an option. *)
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
