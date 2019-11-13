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
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA

 *****************************************************************************)

open Source

class virtual base ~kind ~name (source : source) =
  object
    inherit operator ~name kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    method is_ready = source#is_ready

    method abort_track = source#abort_track
  end

class merge ~kind (source : source) out =
  object
    inherit base ~kind source ~name:"midi.merge_all"

    method private get_frame buf =
      let offset = MFrame.position buf in
      source#get buf ;
      let m = MFrame.content buf offset in
      for c = 0 to Array.length m - 1 do
        MIDI.merge m.(out) m.(c) ;
        if c <> out then MIDI.clear_all m.(c)
      done
  end

class remove ~kind (source : source) t =
  object
    inherit base ~kind source ~name:"midi.remove"

    method private get_frame buf =
      let offset = MFrame.position buf in
      source#get buf ;
      let m = MFrame.content buf offset in
      List.iter (fun c -> if c < Array.length m then MIDI.clear_all m.(c)) t
  end

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_fixed_with ~midi:1 ()) in
  Lang.add_operator "midi.merge_all"
    [ ("track_out", Lang.int_t, Some (Lang.int 0), Some "Destination track.");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.MIDIProcessing
    ~descr:"Merge all MIDI tracks in one."
    (fun p kind ->
      let f v = List.assoc v p in
      let out = Lang.to_int (f "track_out") in
      let src = Lang.to_source (f "") in
      new merge ~kind src out)

let () =
  let k = Lang.kind_type_of_kind_format (Lang.any_fixed_with ~midi:1 ()) in
  Lang.add_operator "midi.remove"
    [ ("", Lang.list_t Lang.int_t, None, Some "Tracks to remove.");
      ("", Lang.source_t k, None, None) ]
    ~kind:(Lang.Unconstrained k) ~category:Lang.MIDIProcessing
    ~descr:"Remove MIDI tracks."
    (fun p kind ->
      (* let f v = List.assoc v p in *)
      let t = List.map Lang.to_int (Lang.to_list (Lang.assoc "" 1 p)) in
      let src = Lang.to_source (Lang.assoc "" 2 p) in
      new remove ~kind src t)
