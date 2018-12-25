(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Encode/decode IFF files, that is AIFF and WAV. *)

type 'a t

type format = [ `Aiff | `Wav ]

val format_of_handler : 'a t -> format

exception Not_a_iff_file of string

type 'a read_ops =
  {
    really_input : 'a -> Bytes.t -> int -> int -> unit ;
    input_byte   : 'a -> int ;
    input        : 'a -> Bytes.t -> int -> int -> int ;
    (* Seek bytes from the _current_ stream position. *)
    seek         : 'a -> int -> unit;
    close        : 'a -> unit
  }

val in_chan_ops :  in_channel read_ops

val fopen : string -> in_channel t
(** Open the named file for reading, and return a new wav descriptor.
   Raise [Sys_error] if the file could not be opened and [Not_a_iff_file]
   if it hasn't the right format. *)

val read_header : 'a read_ops -> 'a -> 'a t
(** Generic opener. *)

val in_chan_read_header : in_channel -> in_channel t
(** Read data from an input channel. *)

val info : 'a t -> string
(** [info w] returns a string containing some informations on wav [w] *)

(** Parameters of the output PCM format. *)
val channels : 'a t -> int
val sample_rate : 'a t -> int
val sample_size : 'a t -> int
val data_length : 'a t -> int

val close : 'a t -> unit
(** [close w] close the wav descriptor [w] *)

(** Returns the WAV header that declares the given format.
  * The lengths of file and data are set to their maximum possible value. *)
val wav_header : ?len:int -> channels:int -> sample_rate:int -> sample_size:int ->
                  unit -> string

(** Returns the duration of the data.
    Warning: value may not be accurate for streams. *)
val duration : 'a t -> float
