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

(** Operations on synthesizers. *)

(** A synthesizer. *)
class type t = object
  (** Set the global volume of the synth. *)
  method set_volume : float -> unit

  (** Play a note. *)
  method note_on : int -> float -> unit

  (** Stop playing a note. *)
  method note_off : int -> float -> unit

  (** Fill a buffer with synthesized data adding to the original data of the
      buffer. *)
  method fill_add : Audio.buffer -> int -> int -> unit

  (** Synthesize into an audio buffer. Notice that the delta times in the track
      should be in samples (so they do depend on the samplerate). *)
  method play : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

  (** Same as [play] but keeps data originally present in the buffer. *)
  method play_add : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit

  (** Reset the synthesizer (sets all notes off in particular). *)
  method reset : unit
end

(** A synthesizer. *)
type synth = t

(** Create a synthesizer from a function which creates a generator at given
    frequency and volume. *)
class create : (float -> float -> Audio.Generator.t) -> t

(** Same as [create] with a mono generator. *)
class create_mono : (float -> float -> Audio.Mono.Generator.t) -> t

(** Sine synthesizer. *)
class sine : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Square synthesizer. *)
class square : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Saw synthesizer. *)
class saw : ?adsr:Audio.Mono.Effect.ADSR.t -> int -> t

(** Synths with only one note at a time. *)
class monophonic : Audio.Generator.t -> t

(** Multichannel synthesizers. *)
module Multitrack : sig
  (** A multichannel synthesizer. *)
  class type t = object
    (** Synthesize into an audio buffer. *)
    method play :
      MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit

    (** Same as [play] but keeps data originally present in the buffer. *)
    method play_add :
      MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit
  end

  (** Create a multichannel synthesizer with given number of channels and a
      function returning the synthesizer on each channel. *)
  class create : int -> (int -> synth) -> t
end
