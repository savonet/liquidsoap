(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
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

open Mm_audio

(** Operations on MIDI data. *)

(** Units for delta-times. *)
type division =
  | Ticks_per_quarter of int  (** Ticks per quarter note. *)
  | SMPTE of int * int  (** SMPTE (frames per second, ticks per frame). *)

type event =
  | Note_off of Audio.Note.t * float
  | Note_on of Audio.Note.t * float
  | Aftertouch of int * float
  | Control_change of int * int
  | Patch of int
  | Channel_aftertouch of int
  | Pitch of int
  (* Meta-events *)
  | Sequence_number of int
  | Text of string
  | Copyright of string
  | Track_name of string
  | Instrument_name of string
  | Lyric of string
  | Marker of string
  | Cue of string
  | End_of_track
  | Tempo of int
  | Time_signature of int * int * int * int
  | Key_signature of int * bool
  | Custom of string

(** {2 Buffers} *)

(** A MIDI buffer. *)
type buffer

val data : buffer -> (int * event) list

(** Create a MIDI buffer of given length in samples. *)
val create : int -> buffer

(** Create a copy of a MIDI buffer. *)
val copy : buffer -> buffer

val blit : buffer -> int -> buffer -> int -> int -> unit
val blit_all : buffer -> buffer -> unit

(** [merge b1 b2] merges the buffer [b2] into [b1]. *)
val merge : buffer -> buffer -> unit

val add : buffer -> int -> buffer -> int -> int -> unit
val clear_all : buffer -> unit
val insert : buffer -> int * event -> unit

(** Multitrack buffers. *)
module Multitrack : sig
  (** A multitrack buffer. *)
  type t = buffer array

  type buffer = t

  (** Channels. *)
  val channels : buffer -> int

  (** Duration. *)
  val duration : buffer -> int

  (** Create a multitrack MIDI buffer with given number of channels and length
      in samples. *)
  val create : int -> int -> buffer

  val clear : ?channel:int -> buffer -> int -> int -> unit
end

module IO : sig
  module Reader : sig
    class type t = object
      (** Read data at with given samplerate for events, in a given track, with
          a given length in samples. *)
      method read : int -> Multitrack.buffer -> int -> int -> int

      (** Close the stream. *)
      method close : unit
    end

    class of_file : string -> t
  end

  module Writer : sig
    class type t = object
      method put : int -> event -> unit
      method note_off : int -> int -> float -> unit
      method note_on : int -> int -> float -> unit
      method advance : int -> unit
      method close : unit
    end

    class to_file : int -> string -> t
  end
end
