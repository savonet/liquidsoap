(*
 * Copyright 2007-2011 Savonet team
 *
 * This file is part of ocaml-ogg.
 *
 * ocaml-ogg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ogg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ogg; if not, write to the Free Software
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

(*
 * Functions for manipulating ogg streams files using libogg.
 *
 * @author Samuel Mimram
 *)

exception Not_enough_data
exception Bad_data
exception Out_of_sync
exception End_of_stream
exception Internal_error

let () =
  Callback.register_exception "ogg_exn_not_enough_data" Not_enough_data;
  Callback.register_exception "ogg_exn_bad_data" Bad_data;
  Callback.register_exception "ogg_exn_out_of_sync" Out_of_sync;
  Callback.register_exception "ogg_exn_eos" End_of_stream;
  Callback.register_exception "ogg_exn_internal_error" Internal_error

module Page = struct
  type t = string * string

  external serialno : t -> nativeint = "ocaml_ogg_page_serialno"
  external eos : t -> bool = "ocaml_ogg_page_eos"
  external bos : t -> bool = "ocaml_ogg_page_bos"
  external packets : t -> int = "ocaml_ogg_page_packets"
  external continued : t -> bool = "ocaml_ogg_page_continued"
  external version : t -> int = "ocaml_ogg_page_version"
  external granulepos : t -> Int64.t = "ocaml_ogg_page_granulepos"
  external pageno : t -> nativeint = "ocaml_ogg_page_pageno"
  external set_checksum : t -> unit = "ocaml_ogg_page_checksum_set"
end

module Stream = struct
  type stream
  type packet

  external packet_granulepos : packet -> Int64.t
    = "ocaml_ogg_stream_packet_granulepos"

  external create : nativeint -> stream = "ocaml_ogg_stream_init"

  let create ?(serial = Random.nativeint (Nativeint.of_int 0x3FFFFFFF)) () =
    create serial

  external serialno : stream -> nativeint = "ocaml_ogg_stream_serialno"
  external eos : stream -> bool = "ocaml_ogg_stream_eos"
  external terminate : stream -> Page.t = "ocaml_ogg_stream_terminate"
  external get_page : stream -> unit -> Page.t = "ocaml_ogg_stream_pageout"
  external get_page_fill : stream -> int -> Page.t = "ocaml_ogg_stream_pageout"

  let get_page ?fill os =
    match fill with
      | Some bytes -> get_page_fill os bytes
      | None -> get_page os ()

  external get_packet : stream -> packet = "ocaml_ogg_stream_packetout"
  external peek_packet : stream -> packet = "ocaml_ogg_stream_packetpeek"

  external peek_granulepos : stream -> Int64.t
    = "ocaml_ogg_stream_granulepospeek"

  external skip_packet : stream -> unit = "ocaml_ogg_stream_packet_advance"
  external put_packet : stream -> packet -> unit = "ocaml_ogg_stream_packetin"
  external put_page : stream -> Page.t -> unit = "ocaml_ogg_stream_pagein"
  external flush_page : stream -> Page.t = "ocaml_ogg_flush_stream"

  let terminate os =
    let rec f pages =
      try f (flush_page os :: pages) with Not_enough_data -> pages
    in
    let pages = f [] in
    List.rev (terminate os :: pages)
end

module Sync = struct
  (** Internal type for sync state *)
  type sync

  type read = bytes -> int -> int -> int

  (** External type for sync state. References the C sync structure, and the
      read function *)
  type t = (read * sync) ref

  external create : unit -> sync = "ocaml_ogg_sync_init"

  let create f = ref (f, create ())

  let create_from_file f =
    let fd = Unix.openfile f [Unix.O_RDONLY] 0o400 in
    (create (Unix.read fd), fd)

  external read : read -> sync -> Page.t = "ocaml_ogg_sync_read"

  let read s =
    let f, s = !s in
    read f s

  external reset : sync -> unit = "ocaml_ogg_sync_reset"

  let reset ?read_func x =
    let f, s = !x in
    reset s;
    match read_func with None -> x := (f, s) | Some v -> x := (v, s)

  external seek : read -> sync -> Page.t = "ocaml_ogg_sync_pageseek"

  let seek x =
    let f, s = !x in
    seek f s
end

module Skeleton = struct
  external fishead :
    Int64.t -> Int64.t -> Int64.t -> Int64.t -> Int32.t -> Stream.packet
    = "ocaml_ogg_skeleton_fishead"

  let fishead ?(presentation_numerator = Int64.zero)
      ?(presentation_denominator = Int64.of_int 1000)
      ?(basetime_numerator = Int64.zero)
      ?(basetime_denominator = Int64.of_int 1000) ?(utc = Int32.zero) () =
    fishead presentation_numerator presentation_denominator basetime_numerator
      basetime_denominator utc

  external eos : unit -> Stream.packet = "ocaml_ogg_skeleton_eos"
end
