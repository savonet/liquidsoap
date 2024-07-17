(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

(** Encode/decode WAV files. *)

type t

exception Not_a_wav_file of string

val fopen : string -> t
(** Open the named wav for reading, and return a new wav descriptor.
   Raise [Sys_error] if the file could not be opened and [Not_a_wav_file]
   if it hasn't the right format. *)

val skip_header : in_channel -> unit

val sample : t -> string -> int -> int -> int
(** [sample w buf pos len] reads up to [len] characters from
   the given wav [w], storing them in string [buf], starting at
   character number [pos].
   It returns the actual number of characters read, between 0 and
   [len] (inclusive).
   A return value of 0 means that the end of file was reached.
   A return value between 0 and [len] exclusive means that
   not all requested [len] characters were read, either because
   no more characters were available at that time, or because
   the implementation found it convenient to do a partial read;
   [sample] must be called again to read the remaining characters,
   if desired.
   Exception [Invalid_argument "input"] is raised if [pos] and [len]
   do not designate a valid substring of [buf]. *)

val info : t -> string
(** [info w] returns a string containing some informations on wav [w] *)

(** Parameters of the output PCM format. *)
val channels : t -> int
val sample_rate : t -> int
val sample_size : t -> int
val big_endian : t -> bool
val signed : t -> bool

val close : t -> unit
(** [close w] close the wav descriptor [w] *)

(** Returns the WAV header that declares the given format.
  * The lengths of file and data are set to their maximum possible value. *)
val header : channels:int -> sample_rate:int -> sample_size:int ->
             big_endian:bool -> signed:bool -> string
