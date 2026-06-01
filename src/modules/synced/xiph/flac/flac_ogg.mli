(*
 * Copyright 2003-2010 Savonet team
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

(** {1 Ogg/flac encoder/decoder modules for OCaml} *)

module Decoder : sig
  (** Check if an ogg packet is the first * packet of an ogg/flac stream. *)
  val check_packet : Ogg.Stream.packet -> bool

  val create :
    fill:(unit -> unit) ->
    write:(float array array -> unit) ->
    Ogg.Stream.stream ->
    Flac.Decoder.t * Flac.Decoder.info * Flac.Decoder.comments option
end

module Encoder : sig
  type t = { encoder : Flac.Encoder.t; first_pages : Ogg.Page.t list }

  val create :
    ?comments:(string * string) list ->
    serialno:Nativeint.t ->
    write:(Ogg.Page.t -> unit) ->
    Flac.Encoder.params ->
    t
end

(** Ogg/flac skeleton module *)
module Skeleton : sig
  (** Generate a flac fisbone packet with * these parameters, to use in an ogg
      skeleton. * Default value for [start_granule] is [Int64.zero], * Default
      value for [headers] is ["Content-type","audio/x-flac"] * * See:
      http://xiph.org/ogg/doc/skeleton.html. *)
  val fisbone :
    ?start_granule:Int64.t ->
    ?headers:(string * string) list ->
    serialno:Nativeint.t ->
    samplerate:Int64.t ->
    unit ->
    Ogg.Stream.packet
end
