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

open Mm

(* Video format converters *)

(** Plugin to add video-related configuration keys. *)
val video_converter_conf : Dtools.Conf.ut

(** [f src dst] performs the conversion from frame [src] to frame [dst]. Raises
    [Not_found] if no conversion routine was found. *)
type converter = Image.Generic.t -> Image.Generic.t -> unit

(** A converter plugin is a name, a list of input formats, a list of output
    formats, a function to create a converter. *)
type converter_plug =
  Image.Generic.Pixel.format list
  * Image.Generic.Pixel.format list
  * (unit -> converter)

(** Plugin to register new converters. *)
val video_converters : converter_plug Plug.t

(** [find_converter source destination] tries to find a converter from source
    format to destination format. Proportional scale is implicitly set via
    global configuration key for now. Returns a conversion function: frame ->
    frame -> unit. *)
val find_converter :
  Image.Generic.Pixel.format ->
  Image.Generic.Pixel.format ->
  Image.Generic.t ->
  Image.Generic.t ->
  unit

(** Generate a function to scale images. *)
val scaler : unit -> Video.Image.t -> Video.Image.t -> unit
