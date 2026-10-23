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

(** The [settings.decoder.ffmpeg.*] keys and the FFmpeg decoder's lists and
    priorities (decoder.md §2, §3.4). Every list and priority is read at each
    use. *)

val max_interleave_duration : float Dtools.Conf.t

(** The decoder name set for a codec, when one is set. Setting a name FFmpeg has
    no decoder for is rejected when it is set. *)
val preferred_decoder : string -> string option

val mime_types : string list Dtools.Conf.t
val image_mime_types : string list Dtools.Conf.t
val file_extensions : string list Dtools.Conf.t
val image_file_extensions : string list Dtools.Conf.t
val priority : int Dtools.Conf.t
val image_priority : int Dtools.Conf.t
val metadata_priority : int Dtools.Conf.t
