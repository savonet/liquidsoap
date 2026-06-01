(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(* Author; Romain Beauxis <toots@rastageeks.org> *)

exception Internal

let () = Callback.register_exception "flac_exn_internal" Internal

module Decoder = struct
  type t

  (** Possible states of a decoder. *)
  type state =
    [ `Search_for_metadata
    | `Read_metadata
    | `Search_for_frame_sync
    | `Read_frame
    | `End_of_stream
    | `Ogg_error
    | `Seek_error
    | `Aborted
    | `Memory_allocation_error
    | `Uninitialized ]

  exception Lost_sync
  exception Bad_header
  exception Frame_crc_mismatch
  exception Unparseable_stream
  exception Not_flac

  let () =
    Callback.register_exception "flac_dec_exn_lost_sync" Lost_sync;
    Callback.register_exception "flac_dec_exn_bad_header" Bad_header;
    Callback.register_exception "flac_dec_exn_crc_mismatch" Frame_crc_mismatch;
    Callback.register_exception "flac_dec_exn_unparseable_stream"
      Unparseable_stream

  type info = {
    sample_rate : int;
    channels : int;
    bits_per_sample : int;
    total_samples : int64;
    md5sum : string;
  }

  type comments = string * (string * string) list
  type comments_array = string * string array

  external info : t -> info * comments_array option = "ocaml_flac_decoder_info"

  let split_comment comment =
    try
      let equal_pos = String.index_from comment 0 '=' in
      let c1 = String.uppercase_ascii (String.sub comment 0 equal_pos) in
      let c2 =
        String.sub comment (equal_pos + 1)
          (String.length comment - equal_pos - 1)
      in
      (c1, c2)
    with Not_found -> (comment, "")

  let _comments cmts =
    match cmts with
      | None -> None
      | Some (vd, cmts) ->
          Some (vd, Array.to_list (Array.map split_comment cmts))

  let info x =
    try
      let info, comments = info x in
      (info, _comments comments)
    with Internal -> raise Not_flac

  external alloc :
    seek:(int64 -> unit) option ->
    tell:(unit -> int64) option ->
    length:(unit -> int64) option ->
    eof:(unit -> bool) option ->
    read:(bytes -> int -> int -> int) ->
    write:(float array array -> unit) ->
    unit ->
    t = "ocaml_flac_decoder_alloc_bytecode" "ocaml_flac_decoder_alloc_native"

  external cleanup : t -> unit = "ocaml_flac_cleanup_decoder"
  external init : t -> unit = "ocaml_flac_decoder_init"

  let create ?seek ?tell ?length ?eof ~read ~write () =
    let write pcm = write (Array.copy pcm) in
    let dec = alloc ~seek ~tell ~length ~eof ~read ~write () in
    Gc.finalise cleanup dec;
    init dec;
    let info, comments = info dec in
    (dec, info, comments)

  external state : t -> state = "ocaml_flac_decoder_state"
  external process : t -> unit = "ocaml_flac_decoder_process"
  external seek : t -> Int64.t -> bool = "ocaml_flac_decoder_seek"
  external flush : t -> bool = "ocaml_flac_decoder_flush"
  external reset : t -> bool = "ocaml_flac_decoder_reset"
  external to_s16le : float array array -> string = "caml_flac_float_to_s16le"

  module File = struct
    type handle = {
      fd : Unix.file_descr;
      dec : t;
      info : info;
      comments : (string * (string * string) list) option;
    }

    let create_from_fd ~write fd =
      let read = Unix.read fd in
      let seek n =
        let n = Int64.to_int n in
        ignore (Unix.lseek fd n Unix.SEEK_SET)
      in
      let tell () = Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR) in
      let length () =
        let stats = Unix.fstat fd in
        Int64.of_int stats.Unix.st_size
      in
      let eof () =
        let stats = Unix.fstat fd in
        Unix.lseek fd 0 Unix.SEEK_CUR = stats.Unix.st_size
      in
      let dec, info, comments =
        create ~seek ~tell ~length ~eof ~write ~read ()
      in
      { fd; comments; dec; info }

    let create ~write filename =
      let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
      try create_from_fd ~write fd
      with e ->
        Unix.close fd;
        raise e
  end
end

module Encoder = struct
  type priv

  type params = {
    channels : int;
    bits_per_sample : int;
    sample_rate : int;
    compression_level : int option;
    total_samples : int64 option;
  }

  type comments = (string * string) list
  type t = priv * params

  exception Invalid_data
  exception Invalid_metadata

  let () =
    Callback.register_exception "flac_enc_exn_invalid_metadata" Invalid_metadata

  external vorbiscomment_entry_name_is_legal : string -> bool
    = "ocaml_flac_encoder_vorbiscomment_entry_name_is_legal"

  external vorbiscomment_entry_value_is_legal : string -> bool
    = "ocaml_flac_encoder_vorbiscomment_entry_value_is_legal"

  external alloc :
    (string * string) array ->
    seek:(int64 -> unit) option ->
    tell:(unit -> int64) option ->
    write:(bytes -> int -> unit) ->
    params ->
    priv = "ocaml_flac_encoder_alloc"

  external cleanup : priv -> unit = "ocaml_flac_cleanup_encoder"
  external init : priv -> unit = "ocaml_flac_encoder_init"

  let create ?(comments = []) ?seek ?tell ~write p =
    if p.channels <= 0 then raise Invalid_data;
    let comments = Array.of_list comments in
    let write b len = write (Bytes.sub b 0 len) in
    let enc = alloc comments ~seek ~tell ~write p in
    Gc.finalise cleanup enc;
    init enc;
    (enc, p)

  external process : priv -> float array array -> int -> unit
    = "ocaml_flac_encoder_process"

  let process (enc, p) data =
    if Array.length data <> p.channels then raise Invalid_data;
    process enc data p.bits_per_sample

  external finish : priv -> unit = "ocaml_flac_encoder_finish"

  let finish (enc, _) = finish enc

  external from_s16le : string -> int -> float array array
    = "caml_flac_s16le_to_float"

  module File = struct
    type handle = { fd : Unix.file_descr; enc : t }

    let create_from_fd ?comments params fd =
      let write s =
        let len = Bytes.length s in
        let rec f pos =
          if pos < len then (
            let ret = Unix.write fd s pos (len - pos) in
            f (pos + ret))
        in
        f 0
      in
      let seek n =
        let n = Int64.to_int n in
        ignore (Unix.lseek fd n Unix.SEEK_SET)
      in
      let tell () = Int64.of_int (Unix.lseek fd 0 Unix.SEEK_CUR) in
      let enc = create ?comments ~seek ~tell ~write params in
      { fd; enc }

    let create ?comments params filename =
      let fd = Unix.openfile filename [Unix.O_CREAT; Unix.O_RDWR] 0o640 in
      create_from_fd ?comments params fd
  end
end
