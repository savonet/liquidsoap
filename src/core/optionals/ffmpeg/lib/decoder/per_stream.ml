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

type item =
  [ `Packet of Ffmpeg_content.copy_packet
  | `Data_packet of [ `Data ] Avcodec.Packet.t
  | `Audio_frame of Avutil.audio Avutil.frame
  | `Video_frame of Avutil.video Avutil.frame
  | `Subtitle_frame of Avutil.Subtitle.frame ]

type outcome = [ `Ok | `Item_fault of string | `Source_fault of string ]

type output = {
  put : Content.data -> unit;
  put_image : fps:Decoder.fps -> Mm.Video.Canvas.image -> unit;
  add_metadata : Frame.metadata -> unit;
}

type t = {
  sparse : bool;
  push : position:int option -> item -> outcome;
  switch : source_id:Source_id.t -> continuing:bool -> unit;
  advance : int -> unit;
  flush : unit -> unit;
}
