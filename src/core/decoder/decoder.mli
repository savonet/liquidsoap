(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Mm

val log : Log.t

type file = string
type stream = string

type input = {
  read : bytes -> int -> int -> int;
  (* Seek to an absolute position in bytes.
     Returns the current position after seeking
     or raises [No_seek] if no seek operation
     is available. *)
  lseek : (int -> int) option;
  tell : (unit -> int) option;
  length : (unit -> int) option;
}

type fps = { num : int; den : int }

(* Buffer passed to decoder. This wraps around
   regular buffer, adding:
    - Implicit resampling
    - Implicit audio channel conversion
    - Implicit video resize
    - Implicit fps conversion
    - Implicit content drop *)
type buffer = {
  generator : Generator.t;
  put_pcm : ?field:Frame.field -> samplerate:int -> Audio.t -> unit;
  put_yuva420p : ?field:Frame.field -> fps:fps -> Video.Canvas.image -> unit;
}

type decoder = {
  decode : buffer -> unit;
  eof : buffer -> unit;
  (* [seek x]: Skip [x] main ticks.
     Returns the number of ticks atcually skipped. *)
  seek : int -> int;
  close : unit -> unit;
}

type file_decoder_ops = {
  fread : int -> Frame.t;
  remaining : unit -> int;
  fseek : int -> int;
  fclose : unit -> unit;
}

type stream_decoder = input -> decoder

type image_decoder = {
  image_decoder_priority : unit -> int;
  check_image : file -> bool;
  decode_image : file -> Video.Image.t;
}

type file_decoder =
  metadata:Frame.metadata ->
  ctype:Frame.content_type ->
  string ->
  file_decoder_ops

type decoder_specs = {
  priority : unit -> int;
  (* None means accept all file extensions. *)
  file_extensions : unit -> string list option;
  (* Mime types are parsed up-to the first ;
     so a file with mime-type foo/bar; bla
     matches mime-type foo/bar. Furthermore,
     for streams, a stream with mime foo/bar<whatever>
     matches mime-type foo/bar.
     None means accept all mime-types. *)
  mime_types : unit -> string list option;
  (* None means no decodable content for that file. *)
  file_type :
    metadata:Frame.metadata ->
    ctype:Frame.content_type ->
    string ->
    Frame.content_type option;
  file_decoder : file_decoder option;
  (* String argument is the full mime-type. *)
  stream_decoder : (ctype:Frame.content_type -> string -> stream_decoder) option;
}

val decoders : decoder_specs Plug.t
val conf_decoder : Dtools.Conf.ut
val conf_mime_types : Dtools.Conf.ut
val conf_file_extensions : Dtools.Conf.ut
val conf_priorities : Dtools.Conf.ut
val conf_image_priorities : Dtools.Conf.ut

(** Open file with readonly, cloexec and share delete on windows. *)
val openfile : string -> Unix.file_descr

(** Test file extension and mime if available *)
val test_file :
  log:Log.t ->
  extension:string option ->
  mime:stream ->
  mimes:stream list option ->
  extensions:string list option ->
  stream ->
  bool

(** Test if we can decode for a content_type. This include cases where we know
    how to convert channel layout. *)
val can_decode_type : Frame.content_type -> Frame.content_type -> bool

val get_file_decoder :
  metadata:Frame.metadata ->
  ctype:Frame.content_type ->
  string ->
  (string * (unit -> file_decoder_ops)) option

val get_stream_decoder :
  ctype:Frame.content_type -> string -> stream_decoder option

val image_file_decoders : image_decoder Plug.t
val check_image_file_decoder : file -> bool
val get_image_file_decoder : file -> Video.Image.t

(* Initialize a decoding buffer *)
val mk_buffer : ctype:Frame.content_type -> Generator.t -> buffer

(* Create a file decoder when remaining time is known. *)
val file_decoder :
  filename:string ->
  remaining:(unit -> int) ->
  ctype:Frame.content_type ->
  decoder ->
  file_decoder_ops

(* Create a file decoder when remaining time is not know,
   in which case it is estimated from consumed bytes during
   the decoding process. *)
val opaque_file_decoder :
  filename:string ->
  ctype:Frame.content_type ->
  (input -> decoder) ->
  file_decoder_ops
