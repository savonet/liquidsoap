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

(** The per-stream decoder contract (decoder.md §7). The decode loop owns
    positions (§8.3): it hands each item with its content position, in main
    ticks on the field, [None] for an item with no timestamp.

    A per-stream decoder never raises out of a call: it answers with an outcome
    the loop acts on. The loop counts [`Item_fault] against the source's fault
    bound; the per-stream decoder resets the count when it delivers an item. *)

type item =
  [ `Packet of Ffmpeg_content.copy_packet
  | `Data_packet of [ `Data ] Avcodec.Packet.t
  | `Audio_frame of Avutil.audio Avutil.frame
  | `Video_frame of Avutil.video Avutil.frame
  | `Subtitle_frame of Avutil.Subtitle.frame ]

type outcome = [ `Ok | `Item_fault of string | `Source_fault of string ]

(** Where a per-stream decoder writes: its field of the decoder's buffer.
    [put_image] goes through the buffer's video entry, which keeps one image per
    video frame at the given rate. *)
type output = {
  put : Content.data -> unit;
  put_image : fps:Decoder.fps -> Mm.Video.Canvas.image -> unit;
  add_metadata : Frame.metadata -> unit;
}

(** [switch]: content put afterwards carries the new source id; data held for
    the previous one is drained first. [advance] fills a sparse field up to a
    position reached by the other streams. [flush] drains everything held. *)
type t = {
  sparse : bool;
  push : position:int option -> item -> outcome;
  switch : source_id:Source_id.t -> continuing:bool -> unit;
  advance : int -> unit;
  flush : unit -> unit;
}
