(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

class virtual base ~name (source : source) =
  object
    inherit operator ~name [source]
    method fallible = source#fallible
    method remaining = source#remaining
    method effective_source = source#effective_source
    method self_sync = source#self_sync
    method private can_generate_frame = source#is_ready
    method abort_track = source#abort_track
  end

class merge (source : source) out =
  object
    inherit base source ~name:"midi.merge_all"

    method private generate_frame =
      let m =
        Content.Midi.get_data (source#get_mutable_content Frame.Fields.midi)
      in
      for c = 0 to Array.length m - 1 do
        MIDI.merge m.(out) m.(c);
        if c <> out then MIDI.clear_all m.(c)
      done;
      source#set_frame_data Frame.Fields.midi Content.Midi.lift_data m
  end

class remove (source : source) t =
  object
    inherit base source ~name:"midi.remove"

    method private generate_frame =
      let m =
        Content.Midi.get_data (source#get_mutable_content Frame.Fields.midi)
      in
      List.iter (fun c -> if c < Array.length m then MIDI.clear_all m.(c)) t;
      source#set_frame_data Frame.Fields.midi Content.Midi.lift_data m
  end

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~midi:(Format_type.midi ()) ())
  in
  Lang.add_operator ~base:Modules.midi "merge_all"
    [
      ("track_out", Lang.int_t, Some (Lang.int 0), Some "Destination track.");
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`MIDI ~descr:"Merge all MIDI tracks in one."
    (fun p ->
      let f v = List.assoc v p in
      let out = Lang.to_int (f "track_out") in
      let src = Lang.to_source (f "") in
      new merge src out)

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~midi:(Format_type.midi ()) ())
  in
  Lang.add_operator ~base:Modules.midi "remove"
    [
      ("", Lang.list_t Lang.int_t, None, Some "Tracks to remove.");
      ("", Lang.source_t frame_t, None, None);
    ]
    ~return_t:frame_t ~category:`MIDI ~descr:"Remove MIDI tracks."
    (fun p ->
      (* let f v = List.assoc v p in *)
      let t = List.map Lang.to_int (Lang.to_list (Lang.assoc "" 1 p)) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new remove src t)
