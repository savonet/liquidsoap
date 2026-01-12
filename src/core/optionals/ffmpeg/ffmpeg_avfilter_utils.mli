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

module Fps : sig
  type t

  val time_base : t -> Avutil.rational

  val init :
    ?start_pts:int64 ->
    width:int ->
    height:int ->
    pixel_format:Avutil.Pixel_format.t ->
    time_base:Avutil.rational ->
    ?pixel_aspect:Avutil.rational ->
    ?source_fps:int ->
    target_fps:int ->
    unit ->
    t

  val convert :
    t -> [ `Video ] Avutil.frame -> ([ `Video ] Avutil.frame -> unit) -> unit

  val eof : t -> ([ `Video ] Avutil.frame -> unit) -> unit
end

module AFormat : sig
  type t

  val time_base : t -> Avutil.rational

  val init :
    ?dst_sample_format:Avutil.Sample_format.t ->
    ?dst_channel_layout:Avutil.Channel_layout.t ->
    ?dst_sample_rate:int ->
    src_sample_format:Avutil.Sample_format.t ->
    src_channel_layout:Avutil.Channel_layout.t ->
    src_sample_rate:int ->
    src_time_base:Avutil.rational ->
    unit ->
    t

  val convert :
    t -> [ `Audio ] Avutil.frame -> ([ `Audio ] Avutil.frame -> unit) -> unit

  val eof : t -> ([ `Audio ] Avutil.frame -> unit) -> unit
end
