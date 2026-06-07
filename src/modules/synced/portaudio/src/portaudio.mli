(*
 * Copyright 2007 Samuel Mimram
 *
 * This file is part of ocaml-portaudio.
 *
 * ocaml-portaudio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-portaudio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-portaudio; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

(**
  * Bindings for the portaudio portable audio library.
  *
  * @author Samuel Mimram
  * @author Niki Yoshiuchi
  *)

(** {2 Exceptions} *)

(** An error occurred. In the future, this exception should be replaced by more
    * specific exceptions. Use [string_of_error] to get a description of the *
    error. *)
exception Error of int

(** An unanticipaced *)
exception Unanticipated_host_error

(** Get a description of an error. *)
val string_of_error : int -> string

(** Get the last error which occurred together with its description. *)
val get_last_host_error : unit -> int * string

(** {2 General} *)

(** Version of portaudio. *)
val get_version : unit -> int

(** Version of portaudio. *)
val get_version_string : unit -> string

(** Initialize the portaudio library. Should be called before calling any other
    * function. *)
val init : unit -> unit

(** Stop using the library. This function should be called before ending the *
    program and no other portaudio function should be called after. *)
val terminate : unit -> unit

(** {2 Host API} *)

(** Host API Information *)
type host_api_info = {
  h_struct_version : int;
  h_host_api_type : int;
  h_name : string;
  h_device_count : int;
  h_default_input_device : int;
  h_default_output_device : int;
}

(** Number of available host API. *)
val get_host_api_count : unit -> int

(** Index of the default host API. *)
val get_default_host_api : unit -> int

(** Information on a host API *)
val get_host_api_info : int -> host_api_info

(** Device Information *)
type device_info = {
  d_struct_version : int;
  d_name : string;
  d_host_api : int;
  d_max_input_channels : int;
  d_max_output_channels : int;
  d_default_low_input_latency : float;
  d_default_low_output_latency : float;
  d_default_high_input_latency : float;
  d_default_high_output_latency : float;
  d_default_sample_rate : float;
}

(** Default input device. *)
val get_default_input_device : unit -> int

(** Default output device. *)
val get_default_output_device : unit -> int

(** Number of available devices. *)
val get_device_count : unit -> int

(** Information on device *)
val get_device_info : int -> device_info

(** {2 Streams} *)

(** The abstract type [('a, 'b) sample_format] describes the OCaml type ['a] and
    the underlying C type ['b] used to represent the data being written to or
    read from a stream. This type is compatible with [('a, 'b) Bigarray.kind].
*)
type ('a, 'b) sample_format

(** See {!Portaudio.format_float32}. *)
val format_int8 : (int, Bigarray.int8_signed_elt) sample_format

(** See {!Portaudio.format_float32}. *)
val format_int16 : (int, Bigarray.int16_signed_elt) sample_format

(** See {!Portaudio.format_float32}. *)
val format_int24 : (int32, Bigarray.int32_elt) sample_format

(** See {!Portaudio.format_float32}. *)
val format_int32 : (int32, Bigarray.int32_elt) sample_format

(** The stream uses floats in the range of [-1.,1.] to represent audio data. *
    The underlying type is a 32 bit float. *)
val format_float32 : (float, Bigarray.float32_elt) sample_format

type ('a, 'b) stream_parameters = {
  channels : int;
  device : int;
  sample_format : ('a, 'b) sample_format;
  latency : float;
}

type stream_flag
type ('a, 'b, 'c, 'd) stream

(** The function signature of a callback. Callbacks only work with interleaved
    streams. *)
type ('a, 'b, 'c, 'd) callback =
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  ('c, 'd, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int

(** [open_stream inparam outparam interleaved rate bufframes callback flags]
    opens a new * stream with input stream of format [inparam], output stream of
    format * [outparam] using interleaved or non-interleaved [interleaved]
    buffers * at [rate] samples per second, with [bufframes] frames per buffer *
    passed the callback function [callback] (0 means leave this choice to *
    portaudio). *)
val open_stream :
  ('a, 'b) stream_parameters option ->
  ('c, 'd) stream_parameters option ->
  ?interleaved:bool ->
  float ->
  int ->
  ?callback:('a, 'b, 'c, 'd) callback ->
  stream_flag list ->
  ('a, 'b, 'c, 'd) stream

(** [open_default_stream callback format interleaved inchans outchans rate
     bufframes] * opens default stream with [callback] as callback function,
    handling samples in * [format] format using interleaved or non-interleaved
    buffers [interleaved] with * [inchans] input channels and [outchans] output
    channels * at [rate] samples per seconds with handling buffers of size
    [bufframes]. *)
val open_default_stream :
  ?callback:('a, 'b, 'a, 'b) callback ->
  ?format:('a, 'b) sample_format ->
  ?interleaved:bool ->
  int ->
  int ->
  int ->
  int ->
  ('a, 'b, 'a, 'b) stream

(** Close a stream. *)
val close_stream : ('a, 'b, 'c, 'd) stream -> unit

(** Start a stream. *)
val start_stream : ('a, 'b, 'c, 'd) stream -> unit

(** Stop a stream. *)
val stop_stream : ('a, 'b, 'c, 'd) stream -> unit

(** Abort a stream. *)
val abort_stream : ('a, 'b, 'c, 'd) stream -> unit

(** Sleep. *)
val sleep : int -> unit

(** Write to a stream. *)
val write_stream :
  ('a, 'b, 'c, 'd) stream -> 'c array array -> int -> int -> unit

(** Read from a stream. *)
val read_stream :
  ('a, 'b, 'c, 'd) stream -> 'a array array -> int -> int -> unit

(** Write to a stream using a bigarray. *)
val write_stream_ba :
  ('a, 'b, 'c, 'd) stream ->
  ('c, 'd, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int ->
  unit

(** Read from a stream using a bigarray. *)
val read_stream_ba :
  ('a, 'b, 'c, 'd) stream ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int ->
  unit

val read_stream_available_frames : ('a, 'b, 'c, 'd) stream -> int
val write_stream_available_frames : ('a, 'b, 'c, 'd) stream -> int
