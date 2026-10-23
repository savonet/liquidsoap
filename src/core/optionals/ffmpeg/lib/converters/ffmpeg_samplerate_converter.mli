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

(** The [ffmpeg] samplerate converter (converters.md §2), registered into
    {!Audio_converter.Samplerate.converters} when the library is linked. A
    converter is created for a channel count, then {!convert} is called on each
    range and {!flush} at every event that ends its input. *)

type t

(** Reads [settings.frame.audio.samplerate] as the input rate. *)
val create : int -> t

(** [convert t ratio data offset length] returns the samples of the resampler
    replaced on a ratio change, then the samples produced. A binding failure
    drops the call's samples and the resampler, with a warning. *)
val convert :
  t ->
  float ->
  Content_audio.data ->
  int ->
  int ->
  Content_audio.data * int * int

(** The samples the resampler holds back. The converter stays usable. *)
val flush : t -> Content_audio.data
