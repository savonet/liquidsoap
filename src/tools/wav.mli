(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2011 Savonet team

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

type 'a t

exception Not_a_wav_file of string

type 'a read_ops = 
  { 
    really_input : 'a -> string -> int -> int -> unit ;
    input_byte   : 'a -> int ;
    input        : 'a -> string -> int -> int -> int ;
    close        : 'a -> unit
  }

val in_chan_ops :  in_channel read_ops

val fopen : string -> in_channel t
(** Open the named wav for reading, and return a new wav descriptor.
   Raise [Sys_error] if the file could not be opened and [Not_a_wav_file]
   if it hasn't the right format. *)

val read_header : 'a read_ops -> 'a -> 'a t
(** Generic WAV opener. *)

val in_chan_read_header : in_channel -> in_channel t
(** Read WAV data from an input channel. *)

val sample : 'a t -> string -> int -> int -> int
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

val info : 'a t -> string
(** [info w] returns a string containing some informations on wav [w] *)

(** Parameters of the output PCM format. *)
val channels : 'a t -> int
val sample_rate : 'a t -> int
val sample_size : 'a t -> int

val close : 'a t -> unit
(** [close w] close the wav descriptor [w] *)

(** Returns the WAV header that declares the given format.
  * The lengths of file and data are set to their maximum possible value. *)
val header : ?len:int -> channels:int -> sample_rate:int -> sample_size:int ->
             unit -> string

(** Returns the duration of the WAV data. 
    Warning: value may not be accurate for 
    streams. *)
val duration : 'a t -> float
