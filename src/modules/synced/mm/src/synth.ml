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

class type t = object
  method set_volume : float -> unit
  method note_on : int -> float -> unit
  method note_off : int -> float -> unit
  method fill_add : Audio.buffer -> int -> int -> unit
  method play_add : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit
  method play : MIDI.buffer -> int -> Audio.buffer -> int -> int -> unit
  method reset : unit
end

type synth = t
type note = { note : int; volume : float; generator : Audio.Generator.t }

class virtual base =
  object (self)
    method virtual private generator : float -> float -> Audio.Generator.t
    val mutable vol : float = 1.
    method set_volume v = vol <- v
    val mutable notes : note list = []

    method note_on n v =
      let note =
        {
          note = n;
          volume = v;
          (* TODO: we could want to change the volume after a not has begun to be played *)
          generator = self#generator (Audio.Note.freq n) (v *. vol);
        }
      in
      notes <- note :: notes

    method note_off n _ =
      (* TODO: remove only one note *)
      (* TODO: merge the two iterations on the list *)
      List.iter (fun note -> if note.note = n then note.generator#release) notes;
      notes <- List.filter (fun note -> not note.generator#dead) notes

    method fill_add buf ofs len =
      List.iter (fun note -> note.generator#fill_add buf ofs len) notes

    method private fill buf ofs len =
      Audio.clear buf ofs len;
      self#fill_add buf ofs len

    method private event =
      function
      | MIDI.Note_off (n, v) -> self#note_off n v
      | MIDI.Note_on (n, v) -> self#note_on n v
      | MIDI.Control_change (0x7, v) -> self#set_volume (float v /. 127.)
      | _ -> ()

    (* TODO: add offset for evs *)
    method play_add evs eofs buf bofs len =
      let rec play o evs ofs =
        match evs with
          | (t, _) :: _ when t >= eofs + len -> ()
          | (t, _) :: tl when t < eofs -> play t tl ofs
          | (t, e) :: tl ->
              let delta = t - max eofs o in
              self#fill_add buf (bofs + ofs) delta;
              self#event e;
              play t tl (ofs + delta)
          | [] -> self#fill_add buf (bofs + ofs) (len - o)
      in
      play 0 (MIDI.data evs) 0

    method play evs eofs buf bofs len =
      Audio.clear buf bofs len;
      self#play_add evs eofs buf bofs len

    method reset = notes <- []
  end

class create g =
  object
    inherit base
    method private generator f v = g f v
  end

class create_mono g = create (fun f v -> new Audio.Generator.of_mono (g f v))

let might_adsr adsr g =
  match adsr with None -> g | Some a -> new Audio.Mono.Generator.adsr a g

class sine ?adsr sr =
  create_mono
    (fun f v -> might_adsr adsr (new Audio.Mono.Generator.sine sr ~volume:v f))

class square ?adsr sr =
  create_mono
    (fun f v ->
      might_adsr adsr (new Audio.Mono.Generator.square sr ~volume:v f))

class saw ?adsr sr =
  create_mono
    (fun f v -> might_adsr adsr (new Audio.Mono.Generator.saw sr ~volume:v f))

class monophonic (g : Audio.Generator.t) =
  object (self)
    method set_volume v = g#set_volume v

    method note_on n v =
      g#set_frequency (Audio.Note.freq n);
      g#set_volume v

    method note_off (_ : int) (_ : float) =
      (* TODO: check for the last note? *)
      g#release

    method fill_add = g#fill_add

    method play_add (_ : MIDI.buffer) (_ : int) (_ : Audio.buffer) (_ : int)
        (_ : int) : unit =
      assert false

    method play evs eofs buf bofs len : unit =
      self#play_add evs eofs buf bofs len;
      assert false

    method reset = g#set_volume 0.
  end

module Multitrack = struct
  class type t = object
    method play_add :
      MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit

    method play :
      MIDI.Multitrack.buffer -> int -> Audio.buffer -> int -> int -> unit
  end

  class create n (f : int -> synth) =
    object (self)
      val synth = Array.init n f

      method play_add (evs : MIDI.Multitrack.buffer) eofs buf bofs len =
        for c = 0 to Array.length synth - 1 do
          synth.(c)#play_add evs.(c) eofs buf bofs len
        done

      method play evs eofs buf bofs len =
        Audio.clear buf bofs len;
        self#play_add evs eofs buf bofs len
    end
end
