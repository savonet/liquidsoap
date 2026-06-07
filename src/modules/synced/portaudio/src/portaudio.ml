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

exception Error of int
exception Unanticipated_host_error

let () =
  Callback.register_exception "portaudio_exn_error" (Error 0);
  Callback.register_exception "portaudio_exn_unanticipated_host_error"
    Unanticipated_host_error

external get_version : unit -> int = "ocaml_pa_get_version"
external get_version_string : unit -> string = "ocaml_pa_get_version_text"
external string_of_error : int -> string = "ocaml_pa_get_error_text"

external get_last_host_error : unit -> int * string
  = "ocaml_pa_get_last_host_error_info"

external init : unit -> unit = "ocaml_pa_initialize"

let init () =
  ignore (Thread.self ());
  init ()

external terminate : unit -> unit = "ocaml_pa_terminate"

type host_api_info = {
  h_struct_version : int;
  h_host_api_type : int;
  h_name : string;
  h_device_count : int;
  h_default_input_device : int;
  h_default_output_device : int;
}

external get_host_api_count : unit -> int = "ocaml_pa_get_host_api_count"
external get_default_host_api : unit -> int = "ocaml_pa_get_default_host_api"
external get_host_api_info : int -> host_api_info = "ocaml_pa_get_host_api_info"

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

external get_default_input_device : unit -> int
  = "ocaml_pa_get_default_input_device"

external get_default_output_device : unit -> int
  = "ocaml_pa_get_default_output_device"

external get_device_count : unit -> int = "ocaml_pa_get_device_count"
external get_device_info : int -> device_info = "ocaml_pa_get_device_info"

(*type sample_format = Format_int8 | Format_int16 | Format_int24 | Format_int32 | Format_float32*)

type ('a, 'b) sample_format = int

let format_int8 = 0
let format_int16 = 1
let format_int24 = 2
let format_int32 = 3
let format_float32 = 4

type ('a, 'b) stream_parameters = {
  channels : int;
  device : int;
  sample_format : ('a, 'b) sample_format;
  latency : float;
}

type stream_flag
type ('a, 'b, 'c, 'd) stream

type ('a, 'b, 'c, 'd) callback =
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  ('c, 'd, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int

external open_stream :
  ('a, 'b) stream_parameters option ->
  ('c, 'd) stream_parameters option ->
  bool ->
  float ->
  int ->
  stream_flag list ->
  ('a, 'b, 'c, 'd) callback option ->
  ('a, 'b, 'c, 'd) stream = "ocaml_pa_open_stream_byte" "ocaml_pa_open_stream"

let open_stream ip op ?(interleaved = true) rate buflen ?callback flags =
  open_stream ip op interleaved rate buflen flags callback

external open_default_stream :
  int ->
  int ->
  ('a, 'b) sample_format ->
  bool ->
  int ->
  int ->
  ('a, 'b, 'a, 'b) callback option ->
  ('a, 'b, 'a, 'b) stream
  = "ocaml_pa_open_default_stream_byte" "ocaml_pa_open_default_stream"

let open_default_stream ?callback ?(format = format_float32)
    ?(interleaved = true) ic oc rate frames =
  open_default_stream ic oc format interleaved rate frames callback

external close_stream : ('a, 'b, 'c, 'd) stream -> unit
  = "ocaml_pa_close_stream"

external start_stream : ('a, 'b, 'c, 'd) stream -> unit
  = "ocaml_pa_start_stream"

external stop_stream : ('a, 'b, 'c, 'd) stream -> unit = "ocaml_pa_stop_stream"

external abort_stream : ('a, 'b, 'c, 'd) stream -> unit
  = "ocaml_pa_abort_stream"

external sleep : int -> unit = "ocaml_pa_sleep"

external write_stream :
  ('a, 'b, 'c, 'd) stream -> 'c array array -> int -> int -> unit
  = "ocaml_pa_write_stream"

external read_stream :
  ('a, 'b, 'c, 'd) stream -> 'a array array -> int -> int -> unit
  = "ocaml_pa_read_stream"

external write_stream_ba :
  ('a, 'b, 'c, 'd) stream ->
  ('c, 'd, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int ->
  unit = "ocaml_pa_write_stream_ba"

external read_stream_ba :
  ('a, 'b, 'c, 'd) stream ->
  ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t ->
  int ->
  int ->
  unit = "ocaml_pa_read_stream_ba"

external read_stream_available_frames : ('a, 'b, 'c, 'd) stream -> int
  = "ocaml_pa_read_stream_available_frames"

external write_stream_available_frames : ('a, 'b, 'c, 'd) stream -> int
  = "ocaml_pa_write_stream_available_frames"
