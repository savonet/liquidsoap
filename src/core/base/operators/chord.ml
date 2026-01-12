(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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
open Source

let chan = 0

let note_of_string = function
  | "-" -> -1 (* mute *)
  | "A" -> 69
  | "A#" | "Bb" -> 70
  | "B" | "Cb" -> 71
  | "C" | "B#" -> 72
  | "C#" | "Db" -> 73
  | "D" -> 74
  | "D#" | "Eb" -> 75
  | "E" | "Fb" -> 76
  | "F" | "E#" -> 77
  | "F#" | "Gb" -> 78
  | "G" -> 79
  | "G#" | "Ab" -> 80
  | _ -> assert false

let note_of_string s = note_of_string s - 12

class chord metadata_name (source : source) =
  object (self)
    inherit operator ~name:"chord" [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    val mutable notes_on = []

    method private generate_frame =
      let buf = source#get_mutable_content Frame.Fields.midi in
      let m = Content.Midi.get_data buf in
      let meta = Frame.get_all_metadata source#get_frame in
      let chords =
        let ans = ref [] in
        List.iter
          (fun (t, m) ->
            match Frame.Metadata.find_opt metadata_name m with
              | None -> ()
              | Some c -> (
                  try
                    let sub =
                      Re.Pcre.exec
                        ~rex:
                          (Re.Pcre.regexp "^([A-G-](?:b|#)?)(|M|m|M7|m7|dim)$")
                        c
                    in
                    let n = Re.Pcre.get_substring sub 1 in
                    let n = note_of_string n in
                    let m = Re.Pcre.get_substring sub 2 in
                    ans := (t, n, m) :: !ans
                  with Not_found ->
                    self#log#important "Could not parse chord '%s'." c))
          meta;
        List.rev !ans
      in
      let play t n =
        List.iter (fun n -> MIDI.insert m.(chan) (t, MIDI.Note_on (n, 1.))) n;
        notes_on <- n @ notes_on
      in
      let mute t =
        List.iter
          (fun n -> MIDI.insert m.(chan) (t, MIDI.Note_off (n, 1.)))
          notes_on;
        notes_on <- []
      in
      List.iter
        (fun (t, c, m) ->
          (* time, base, mode *)
          mute t;

          (* Negative base note means mute. *)
          if c >= 0 then (
            match m with
              | "" | "M" ->
                  (* major *)
                  play t [c; c + 4; c + 7]
              | "m" ->
                  (* minor *)
                  play t [c; c + 3; c + 7]
              | "7" -> play t [c; c + 4; c + 7; c + 10]
              | "M7" -> play t [c; c + 4; c + 7; c + 11]
              | "m7" -> play t [c; c + 3; c + 7; c + 10]
              | "dim" -> play t [c; c + 3; c + 6]
              | m -> self#log#debug "Unknown mode: %s\n%!" m))
        chords;
      source#set_frame_data Frame.Fields.midi Content.Midi.lift_data m
  end

let _ =
  (* TODO: is this really the type we want to give to it? *)
  let in_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  let out_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~midi:(Format_type.midi_n 1) ())
  in
  Lang.add_operator ~base:Modules.midi "chord"
    [
      ( "metadata",
        Lang.string_t,
        Some (Lang.string "chord"),
        Some "Name of the metadata containing the chords." );
      ("", Lang.source_t in_t, None, None);
    ]
    ~return_t:out_t ~category:`MIDI ~descr:"Generate a chord."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      let metadata = Lang.to_string (f "metadata") in
      (new chord metadata src :> Source.source))
