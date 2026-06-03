(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-vorbis.
 *
 * Ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Decode from or encode to the Ogg Vorbis compressed audio format; or get
  * information about an Ogg Vorbis file.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

exception Invalid_parameters
exception Invalid_channels
exception Could_not_open_file
exception Not_vorbis
exception Hole_in_data
exception Bad_link
exception Version_mismatch
exception Bad_header
exception Read_error
exception Internal_fault
exception Invalid_argument
exception Not_implemented
exception Unknown_error of int
exception Not_audio
exception False
exception Utf8_failure of string

(* New register exception printer *)
let string_of_exc e =
  let f s = Some s in
  match e with
    | Invalid_parameters -> f "Invalid vorbis parameters"
    | Invalid_channels -> f "Invalid vorbis channels"
    | Could_not_open_file -> f "Vorbis: could not open file"
    | Not_vorbis -> f "Bitstream is not Vorbis data"
    | Hole_in_data ->
        f
          "Interruption in the vorbis data (one of: garbage between pages, \
           loss of sync followed by recapture, or a corrupt page)"
    | Bad_link ->
        f
          "An invalid vorbis stream section was supplied, or the requested \
           link is corrupt"
    | Version_mismatch -> f "Vorbis bitstream version mismatch"
    | Bad_header -> f "Invalid Vorbis bitstream header"
    | Read_error -> f "Vorbis: read error"
    | Internal_fault ->
        f
          "Internal vorbis logic fault; indicates a bug or heap/stack \
           corruption"
    | Invalid_argument ->
        f "Invalid vorbis setup request, e.g. out of range argument"
    | Not_implemented ->
        f
          "Unimplemented vorbis feature (e.g. -0.2 quality is only available \
           in aoTuV's implementation)"
    | Unknown_error i ->
        f
          (Printf.sprintf
             "Unknown vorbis error %i (it should not have happened, please \
              report)"
             i)
    | Not_audio -> f "Ogg packet doesn't contain audio data"
    | False ->
        f
          "Vorbis call returned a 'false' status (eg, playback is not in \
           progress, and thus there is no instantaneous bitrate information to \
           report.)"
    | Utf8_failure s -> f (Printf.sprintf "UTF8 failure in string: %S" s)
    | _ -> None

let () = Printexc.register_printer string_of_exc

let _ =
  Callback.register_exception "vorbis_exn_invalid_parameters" Invalid_parameters;
  Callback.register_exception "vorbis_exn_invalid_channels" Invalid_channels;
  Callback.register_exception "vorbis_exn_could_not_open_file"
    Could_not_open_file;
  Callback.register_exception "vorbis_exn_not_vorbis" Not_vorbis;
  Callback.register_exception "vorbis_exn_hole_in_data" Hole_in_data;
  Callback.register_exception "vorbis_exn_bad_link" Bad_link;
  Callback.register_exception "vorbis_exn_version_mismatch" Version_mismatch;
  Callback.register_exception "vorbis_exn_bad_header" Bad_header;
  Callback.register_exception "vorbis_exn_read_error" Read_error;
  Callback.register_exception "vorbis_exn_internal_fault" Internal_fault;
  Callback.register_exception "vorbis_exn_invalid" Invalid_argument;
  Callback.register_exception "vorbis_exn_not_implemented" Not_implemented;
  Callback.register_exception "vorbis_exn_not_audio" Not_audio;
  Callback.register_exception "vorbis_exn_unknown_error" (Unknown_error 0);
  Callback.register_exception "vorbis_exn_false" False;
  Callback.register_exception "vorbis_exn_utf8_failure" (Utf8_failure "")

let tags m () =
  let ans = ref [] in
  let add t v = ans := (t, v) :: !ans in
  Hashtbl.iter add m;
  List.rev !ans

let encoder_tag = "ocaml-vorbis by the savonet team (http://savonet.sf.net/)"

module Encoder = struct
  type t

  external create : int -> int -> int -> int -> int -> t
    = "ocaml_vorbis_analysis_init"

  external create_vbr : int -> int -> float -> t
    = "ocaml_vorbis_analysis_init_vbr"

  external reset : t -> unit = "ocaml_vorbis_reset"

  external headerout_packetout :
    t ->
    (string * string) array ->
    Ogg.Stream.packet * Ogg.Stream.packet * Ogg.Stream.packet
    = "ocaml_vorbis_analysis_headerout"

  let headerout_packetout ?(encoder = encoder_tag) state tags =
    let tags = Array.of_list (tags @ [("ENCODER", encoder)]) in
    headerout_packetout state tags

  let headerout ?encoder state os tags =
    let p1, p2, p3 = headerout_packetout ?encoder state tags in
    Ogg.Stream.put_packet os p1;
    Ogg.Stream.put_packet os p2;
    Ogg.Stream.put_packet os p3

  external get_channels : t -> int = "ocaml_vorbis_encode_get_channels"

  external encode_buffer_float :
    t -> Ogg.Stream.stream -> float array array -> int -> int -> unit
    = "ocaml_vorbis_encode_float"

  external encode_buffer_float_ba :
    t ->
    Ogg.Stream.stream ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int ->
    int ->
    unit = "ocaml_vorbis_encode_float_ba"

  external time_of_granulepos : t -> Int64.t -> Nativeint.t
    = "ocaml_vorbis_encode_time_of_granulepos"

  (* We encode a buffer with 0 samples to finish
   * the stream, according to the documentation of
   * vorbis_analysis_wrote:
   * "A value of zero means all input data has been provided and
   * the compressed stream should be finalized." *)
  let end_of_stream enc os =
    let chans = get_channels enc in
    let data = Array.make chans [||] in
    encode_buffer_float enc os data 0 0
end

let split_comment comment =
  try
    let equal_pos = String.index_from comment 0 '=' in
    let c1 = String.uppercase_ascii (String.sub comment 0 equal_pos) in
    let c2 =
      String.sub comment (equal_pos + 1) (String.length comment - equal_pos - 1)
    in
    (c1, c2)
  with Not_found -> (comment, "")

type bitstream = int

type info = {
  vorbis_version : int;
  audio_channels : int;
  audio_samplerate : int;
  bitrate_upper : int;
  bitrate_nominal : int;
  bitrate_lower : int;
  bitrate_window : int;
}

module File = struct
  module Decoder = struct
    type t

    external create :
      (int -> string * int) ->
      (int -> Unix.seek_command -> int) ->
      (unit -> int) ->
      t = "ocaml_vorbis_open_dec_stream"

    let openfile_with_fd fd =
      try
        create
          (fun n ->
            let buf = Bytes.create n in
            let r = Unix.read fd buf 0 n in
            (Bytes.unsafe_to_string buf, r))
          (fun n cmd -> Unix.lseek fd n cmd)
          (fun () -> Unix.lseek fd 0 Unix.SEEK_CUR)
      with e ->
        Unix.close fd;
        raise e

    let openfile f =
      let fd = Unix.openfile f [Unix.O_RDONLY] 0o400 in
      (openfile_with_fd fd, fd)

    external decode_float : t -> float array array -> int -> int -> int
      = "ocaml_vorbis_decode_float"

    external decode_float_alloc : t -> int -> float array array
      = "ocaml_vorbis_decode_float_alloc"

    external decode_float_ba :
      t ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
      int ->
      int ->
      int = "ocaml_vorbis_decode_float_ba"

    external decode_float_alloc_ba :
      t ->
      int ->
      (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array
      = "ocaml_vorbis_decode_float_alloc_ba"

    external decode : t -> bool -> int -> bool -> bytes -> int -> int -> int
      = "ocaml_vorbis_decode_byte" "ocaml_vorbis_decode"

    let decode df ?(big_endian = false) ?(sample_size = 2) ?(signed = true) buf
        ofs len =
      decode df big_endian sample_size signed buf ofs len

    external bitstream : t -> int = "ocaml_vorbis_get_dec_file_bitstream"

    external comments : t -> int -> string * string array
      = "ocaml_vorbis_get_dec_file_comments"

    let comments df bitstream =
      let vd, cmts = comments df bitstream in
      (vd, Array.to_list (Array.map split_comment cmts))

    external info : t -> int -> info = "ocaml_vorbis_decoder_info"
    external bitrate : t -> int -> int = "ocaml_vorbis_decoder_bitrate"
    external duration : t -> int -> float = "ocaml_vorbis_decoder_time_total"
    external streams : t -> int = "ocaml_vorbis_decoder_streams"

    external serialnumber : t -> int -> int
      = "ocaml_vorbis_decoder_serialnumber"

    external samples : t -> int -> int = "ocaml_vorbis_decoder_pcm_total"
  end
end

module Decoder = struct
  type t

  external init :
    Ogg.Stream.packet -> Ogg.Stream.packet -> Ogg.Stream.packet -> t
    = "ocaml_vorbis_synthesis_init"

  external info : t -> info = "ocaml_vorbis_val_info_of_decoder"

  external comments : t -> string * string array
    = "ocaml_vorbis_val_comments_of_decoder"

  let comments dec =
    let vend, cmts = comments dec in
    (vend, Array.to_list (Array.map split_comment cmts))

  external check_packet : Ogg.Stream.packet -> bool
    = "ocaml_vorbis_check_packet"

  external decode_pcm :
    t -> Ogg.Stream.stream -> float array array -> int -> int -> int
    = "ocaml_vorbis_decode_pcm"

  external decode_pcm_ba :
    t ->
    Ogg.Stream.stream ->
    (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array ->
    int ->
    int ->
    int = "ocaml_vorbis_decode_pcm_ba"

  external restart : t -> unit = "ocaml_vorbis_synthesis_restart"
end

module Skeleton = struct
  external fisbone :
    Nativeint.t -> Int64.t -> Int64.t -> string -> Ogg.Stream.packet
    = "ocaml_vorbis_skeleton_fisbone"

  let fisbone ?(start_granule = Int64.zero)
      ?(headers = [("Content-type", "audio/vorbis")]) ~serialno ~samplerate () =
    let concat s (h, v) = Printf.sprintf "%s%s: %s\r\n" s h v in
    let s = List.fold_left concat "" headers in
    fisbone serialno samplerate start_granule s
end
