(*
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of Ocaml-mad.
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Functions for decoding mp3 files using the libmad.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

(** {1 Exceptions} *)

(** An error occurred with libmad. *)
exception Mad_error of string

(** An error occurred while reading a file. *)
exception Read_error of string

(** The end of the mp3 stream was reached. *)
exception End_of_stream

(** Could not open a file. *)
exception Openfile_error of string

(** Could not close a file. *)
exception Closefile_error of string

(** {1 Decoding files} *)

(** An mp3 file opened for decoding. *)
type mad_file

type mpeg_layer = Layer_I | Layer_II | Layer_III
type emphasis = None | MS_50_15 | CCITT_J_17 | Reserved
type channel_mode = Single_channel | Dual_channel | Joint_stereo | Stereo

type frame_format = {
  layer : mpeg_layer;
  mode : channel_mode;
  emphasis : emphasis;
  bitrate : int;
  samplerate : int;
  channels : int;
  samples_per_channel : int;
  original : bool;
  copyright : bool;
  private_bit : bool;
}

(** Type for abstract reader. *)
type read = bytes -> int -> int -> int

(**
  * Open an mp3 file.
  *
  * @raise Openfile_error if an error occurred while trying to open the file.
  *)
val openfile : string -> mad_file

(** * [openstream read_func] opens a stream where [read_func n] should be a *
    function which returns [n] bytes of data or less, the second component of *
    the result being the number of bytes to read in the fist component. *)
val openstream : read -> mad_file

(** * Skip ID3 tags that may be present at * the beginning of a stream. This
    function * may be used to a mp3 file opened using [openstream]. * ID3 tags
    are always skipped when using [openfile]. * * [seek] is a callback to seek
    to an absolute * position on the encoded data, and [tell] a callback * to
    fetch the current position. [read] is the reading * callback. *)
val skip_id3tags : read:read -> seek:(int -> int) -> tell:(unit -> int) -> unit

(**
  * Close an mp3 file previously opened with [openfile].
  *
  * @raise Closefile_error if an error occurred while trying to close the file.
  *)
val close : mad_file -> unit

(** * Get the current position (in bytes) of the decoder in the mp3 file which *
    should have been opened with [openfile]. *)
val get_current_position : mad_file -> int

type time_unit =
  | Hours
  | Minutes
  | Seconds
  | Deciseconds
  | Centiseconds
  | Milliseconds

(** * Get the current time position (in the given unit) of the decoder. *)
val get_current_time : mad_file -> time_unit -> int

(** Decode an mp3 frame. * Returned data in interleaved when * there are two
    channels, and mono data * when there is only one. *)
val decode_frame : mad_file -> string

(** Decode an mp3 frame. *)
val decode_frame_float : mad_file -> float array array

(** Decode an mp3 frame. *)
val decode_frame_floatarray : mad_file -> floatarray array

val decode_frame_float_ba :
  mad_file ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

(** Skip one frame. The current time/position is * updated but the frame is not
    decoded. *)
val skip_frame : mad_file -> unit

(*
 * Get the format of the latest decoded frame. This should be called after [decode_frame] or
 * [decode_frame_float].
 *)
val get_frame_format : mad_file -> frame_format

(* This function is DEPRECATED. Please use [get_frame_format] instead. *)
val get_output_format : mad_file -> int * int * int

(** Compute the duration of a file, in seconds. * Never raises any exception,
    but returns 0. in case of error. *)
val duration : string -> float
