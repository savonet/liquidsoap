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

(** The internal audio per-stream decoder (decoder-internal.md §1–§8, audio):
    decoded frames resampled to the audio rate and the format's channel count,
    in the format's PCM kind, put straight onto the field. [create] raises
    [Failure] for a PCM kind it has no converter for. *)

val create :
  output:Per_stream.output ->
  faults:Faults.t ->
  log:Log.t ->
  pcm_kind:Content_base.kind ->
  channels:int ->
  Avutil.audio Avcodec.params ->
  Per_stream.t
