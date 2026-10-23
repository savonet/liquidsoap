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

(** One [%ffmpeg] encoder instance (encoder.md): the container, its streams in
    track order, options, encoding, metadata, stop and container restarts. It is
    registered as the encoder plug-in of [%ffmpeg] formats when the layer is
    linked. *)

val create :
  ?hls:bool ->
  pos:Pos.t option ->
  name:string ->
  Frame.Metadata.Export.t ->
  Liquidsoap_ffmpeg_formats.Encoder_format.t ->
  Encoder.encoder
