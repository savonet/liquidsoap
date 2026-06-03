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
  * Functions for decoding mp3 files using libmad.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

type read = bytes -> int -> int -> int
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

exception Mad_error of string
exception Read_error of string
exception End_of_stream
exception Openfile_error of string
exception Closefile_error of string

let _ =
  Callback.register_exception "mad_exn_mad_error" (Mad_error "");
  Callback.register_exception "mad_exn_read_error" (Read_error "");
  Callback.register_exception "mad_exn_end_of_stream" End_of_stream;
  Callback.register_exception "mad_exn_openfile_error" (Openfile_error "");
  Callback.register_exception "mad_exn_closefile_error" (Closefile_error "")

external openfile : string -> mad_file = "ocaml_mad_openfile"
external openstream : read -> mad_file = "ocaml_mad_openstream"

external skip_id3tags : read -> (int -> int) -> (unit -> int) -> unit
  = "ocaml_mad_skip_id3tag"

let skip_id3tags ~read ~seek ~tell = skip_id3tags read seek tell

external close : mad_file -> unit = "ocaml_mad_close"

external get_current_position : mad_file -> int
  = "ocaml_mad_get_current_position"

external get_current_time : mad_file -> int -> int = "ocaml_mad_time"

type time_unit =
  | Hours
  | Minutes
  | Seconds
  | Deciseconds
  | Centiseconds
  | Milliseconds

let int_of_time_unit = function
  | Hours -> -2
  | Minutes -> -1
  | Seconds -> 0
  | Deciseconds -> 10
  | Centiseconds -> 100
  | Milliseconds -> 1000

let get_current_time dec u = get_current_time dec (int_of_time_unit u)

external skip_frame : mad_file -> unit = "ocaml_mad_skip_frame"
external decode_frame : mad_file -> string = "ocaml_mad_decode_frame"

external decode_frame_float : mad_file -> float array array
  = "ocaml_mad_decode_frame_float"

external decode_frame_floatarray : mad_file -> floatarray array
  = "ocaml_mad_decode_frame_floatarray"

external decode_frame_float_ba :
  mad_file ->
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array
  = "ocaml_mad_decode_frame_float_ba"

external get_frame_format : mad_file -> frame_format
  = "ocaml_mad_get_frame_format"

let get_output_format mf =
  let header = get_frame_format mf in
  (header.samplerate, header.channels, header.samples_per_channel)

let duration file =
  let mf = openfile file in
  let close () = try close mf with _ -> () in
  try
    begin try
      while true do
        skip_frame mf
      done
    with _ -> ()
    end;
    let ret = float (get_current_time mf Centiseconds) /. 100. in
    close ();
    ret
  with _ ->
    close ();
    0.
