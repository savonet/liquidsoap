(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(* Video format converters *)

(** Plugin to add video-related configuration keys. *)
val video_converter_conf : Dtools.Conf.ut

(** [~proportional src dst] performs the
  * conversion from frame src to frame dst.
  * raises Not_found if no conversion routine
  * was found *)
type converter = proportional:bool -> Image.Generic.t -> Image.Generic.t -> unit

(** A converter plugin is a name, a list of input formats,
  * a list of output formats,
  * a fonction to create a converter. *)
type converter_plug =
  Image.Generic.Pixel.format list
  * Image.Generic.Pixel.format list
  * (unit -> converter)

(** Plugin to register new converters. *)
val video_converters : converter_plug Plug.plug

(** [find_converter source destination] tries
  * to find a converter from source format
  * to destination format. Proportional scale
  * is implicitely set via global configuration key
  * for now. Returns a conversion function: frame -> frame -> unit. *)
val find_converter :
  Image.Generic.Pixel.format ->
  Image.Generic.Pixel.format ->
  Image.Generic.t ->
  Image.Generic.t ->
  unit

val scaler :
  unit -> ?proportional:bool -> Video.Image.t -> Video.Image.t -> unit
