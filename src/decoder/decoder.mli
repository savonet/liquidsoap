(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

val log : Dtools.Log.t

type file = string
type stream = string

type input = int -> string * int

type 'a decoder = Decoder of ('a -> unit)
type stream_decoder = input -> Generator.From_audio_video_plus.t decoder
type file_decoder = { fill : Frame.t -> int; close : unit -> unit; }

val file_decoders :
  (metadata:Frame.metadata -> file -> Frame.content_kind ->
     (unit -> file_decoder) option)
  Plug.plug
val stream_decoders :
  (stream -> Frame.content_kind -> stream_decoder option) Plug.plug

val conf_mime_types : Dtools.Conf.ut
val conf_file_extensions : Dtools.Conf.ut

(** Test file extension and mime 
  * if available *)
val test_file : ?log:Dtools.Log.t ->
                mimes:string list -> 
                extensions:string list ->
                string -> bool

val get_file_decoder :
  metadata:Frame.metadata -> file -> Frame.content_kind ->
  (string * (unit -> file_decoder)) option
val get_stream_decoder :
  file -> Frame.content_kind -> stream_decoder option

module Buffered :
  functor (Generator : Generator.S) ->
    sig
      val file_decoder :
        file ->
        Frame.content_kind ->
        (input -> Generator.t decoder) ->
        Generator.t ->
        file_decoder
    end
