(*
 * Copyright 2013 Savonet team
 *
 * This file is part of ocaml-fdkaac.
 *
 * ocaml-fdkaac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-fdkaac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-fdkaac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** OCaml bindings for the libfdk-aac. *)

module Encoder = struct
  exception Invalid_handle
  exception Unsupported_parameter
  exception Invalid_config
  exception Invalid_data
  exception Error of int
  exception End_of_file
  exception Unknown of int

  let () =
    Callback.register_exception "fdkaac_exn_invalid_handle" Invalid_handle;
    Callback.register_exception "fdkaac_exn_unsupported_parameter"
      Unsupported_parameter;
    Callback.register_exception "fdkaac_exn_invalid_config" Invalid_config;
    Callback.register_exception "fdkaac_exn_error" (Error 0);
    Callback.register_exception "fdkaac_exn_encode_eof" End_of_file;
    Callback.register_exception "aacplus_exn_unknown_error" (Unknown 0)

  let string_of_exception = function
    | Invalid_handle -> Some "Handle passed to function call was invalid"
    | Unsupported_parameter -> Some "Parameter not available"
    | Invalid_config -> Some "Invalid configuration"
    | Invalid_data -> Some "Invalid input data"
    | Error 0 -> Some "General initialization error"
    | Error 1 -> Some "AAC library initialization error"
    | Error 2 -> Some "SBR library initialization error"
    | Error 3 -> Some "Transport library initialization error"
    | Error 4 -> Some "Meta data library initialization error"
    | Error 5 ->
        Some "The encoding process was interrupted by an unexpected error"
    | End_of_file -> Some "End of file reached"
    | _ -> None

  let () = Printexc.register_printer string_of_exception

  type enc
  type info = { input_channels : int; frame_length : int }

  external info : enc -> info = "ocaml_fdkaac_info"

  type t = { enc : enc; mutable info : info option; buffer : Buffer.t }

  external create : int -> enc = "ocaml_fdkaac_init_enc"

  let create chans =
    { enc = create chans; info = None; buffer = Buffer.create 1024 }

  type mpeg2_aac = [ `AAC_LC | `HE_AAC | `HE_AAC_v2 ]
  type mpeg4_aac = [ mpeg2_aac | `AAC_LD | `AAC_ELD ]
  type aot = [ `Mpeg_4 of mpeg4_aac | `Mpeg_2 of mpeg2_aac ]

  let int_of_aot = function
    | `Mpeg_4 `AAC_LC -> 2
    | `Mpeg_4 `HE_AAC -> 5
    | `Mpeg_4 `HE_AAC_v2 -> 29
    | `Mpeg_4 `AAC_LD -> 23
    | `Mpeg_4 `AAC_ELD -> 39
    | `Mpeg_2 `AAC_LC -> 129
    | `Mpeg_2 `HE_AAC -> 132
    | `Mpeg_2 `HE_AAC_v2 -> 156

  let aot_of_int = function
    | 2 -> `Mpeg_4 `AAC_LC
    | 5 -> `Mpeg_4 `HE_AAC
    | 29 -> `Mpeg_4 `HE_AAC_v2
    | 23 -> `Mpeg_4 `AAC_LD
    | 39 -> `Mpeg_4 `AAC_ELD
    | 129 -> `Mpeg_2 `AAC_LC
    | 132 -> `Mpeg_2 `HE_AAC
    | 156 -> `Mpeg_2 `HE_AAC_v2
    | _ -> raise Unsupported_parameter

  type bitrate_mode = [ `Constant | `Full_bitreservoir | `Variable of int ]

  let int_of_bitrate_mode = function
    | `Constant -> 0
    | `Variable vbr -> vbr
    | `Full_bitreservoir -> 8

  let bitrate_mode_of_int = function
    | 0 -> `Constant
    | 8 -> `Full_bitreservoir
    | vbr -> `Variable vbr

  type transmux = [ `Raw | `Adif | `Adts | `Latm | `Latm_out_of_band | `Loas ]

  let int_of_transmux = function
    | `Raw -> 0
    | `Adif -> 1
    | `Adts -> 2
    | `Latm -> 6
    | `Latm_out_of_band -> 7
    | `Loas -> 10

  let transmux_of_int = function
    | 0 -> `Raw
    | 1 -> `Adif
    | 2 -> `Adts
    | 6 -> `Latm
    | 7 -> `Latm_out_of_band
    | 10 -> `Loas
    | _ -> raise Unsupported_parameter

  type param =
    [ `Aot of aot
    | `Bitrate of int
    | `Bitrate_mode of bitrate_mode
    | `Samplerate of int
    | `Sbr_mode of bool
    | `Granule_length of int
    | `Afterburner of bool
    | `Bandwidth of int
    | `Transmux of transmux ]

  type param_name =
    [ `Aot
    | `Bitrate
    | `Bitrate_mode
    | `Samplerate
    | `Sbr_mode
    | `Granule_length
    | `Afterburner
    | `Bandwidth
    | `Transmux ]

  let int_of_param_name = function
    | `Aot -> 0x0100
    | `Bitrate -> 0x0101
    | `Bitrate_mode -> 0x0102
    | `Samplerate -> 0x0103
    | `Sbr_mode -> 0x0104
    | `Granule_length -> 0x0105
    | `Afterburner -> 0x0200
    | `Bandwidth -> 0x0203
    | `Transmux -> 0x0300

  let extract_param = function
    | `Aot x -> (int_of_param_name `Aot, int_of_aot x)
    | `Bitrate x -> (int_of_param_name `Bitrate, x)
    | `Bitrate_mode x -> (int_of_param_name `Bitrate_mode, int_of_bitrate_mode x)
    | `Samplerate x -> (int_of_param_name `Samplerate, x)
    | `Sbr_mode x -> (int_of_param_name `Sbr_mode, if x then 1 else 0)
    | `Granule_length x -> (int_of_param_name `Granule_length, x)
    | `Afterburner x -> (int_of_param_name `Afterburner, if x then 1 else 0)
    | `Bandwidth x -> (int_of_param_name `Bandwidth, x)
    | `Transmux x -> (int_of_param_name `Transmux, int_of_transmux x)

  external set : enc -> int -> int -> unit = "ocaml_fdkaac_set_param"

  let set enc param =
    let p, v = extract_param param in
    set enc.enc p v

  let pack_param = function
    | `Aot, x -> `Aot (aot_of_int x)
    | `Bitrate, x -> `Bitrate x
    | `Bitrate_mode, x -> `Bitrate_mode (bitrate_mode_of_int x)
    | `Samplerate, x -> `Samplerate x
    | `Sbr_mode, x -> `Sbr_mode (x == 1)
    | `Granule_length, x -> `Granule_length x
    | `Afterburner, x -> `Afterburner (x == 1)
    | `Bandwidth, x -> `Bandwidth x
    | `Transmux, x -> `Transmux (transmux_of_int x)

  external get : enc -> int -> int = "ocaml_fdkaac_get_param"

  let get enc param =
    let x = get enc.enc (int_of_param_name param) in
    pack_param (param, x)

  external encode : enc -> string -> int -> int -> string
    = "ocaml_fdkaac_encode"

  (* Drop the first [len] bytes. *)
  let buffer_drop buffer len =
    let size = Buffer.length buffer in
    if len = size then Buffer.clear buffer
    else begin
      let tmp = Buffer.sub buffer len (size - len) in
      Buffer.clear buffer;
      Buffer.add_string buffer tmp
    end

  let rec encode_frames enc len pos out =
    if len <= Buffer.length enc.buffer - pos then begin
      let data = Buffer.sub enc.buffer pos len in
      Buffer.add_string out (encode enc.enc data 0 len);
      encode_frames enc len (pos + len) out
    end
    else if 0 < pos then buffer_drop enc.buffer pos

  let get_info enc =
    match enc.info with
      | Some info -> info
      | None ->
          let info = info enc.enc in
          enc.info <- Some info;
          info

  let chunk_length enc =
    let info = get_info enc in
    2 * info.input_channels * info.frame_length

  let encode enc data ofs len =
    if String.length data - ofs < len then raise Invalid_data;
    Buffer.add_substring enc.buffer data ofs len;
    let ret = Buffer.create 1024 in
    encode_frames enc (chunk_length enc) 0 ret;
    Buffer.contents ret

  external flush : enc -> string = "ocaml_fdkaac_flush"

  let flush enc =
    let buf = Buffer.create 1024 in
    let len = chunk_length enc in
    encode_frames enc len 0 buf;
    let rem = Buffer.length enc.buffer in
    if 0 < rem then begin
      Buffer.add_string enc.buffer (String.make (len - rem) (Char.chr 0));
      encode_frames enc len 0 buf
    end;
    try
      while true do
        Buffer.add_string buf (flush enc.enc)
      done;
      assert false
    with Unsupported_parameter | End_of_file -> Buffer.contents buf
end
