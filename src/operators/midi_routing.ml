(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

open Source

class virtual base (source:source) =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track
end

class merge (source:source) out =
object (self)
  inherit base (source)

  method private get_frame buf =
    source#get buf;
    let m = MFrame.tracks buf in
      for c = 0 to Array.length m - 1 do
        m.(out) := !(m.(c)) @ !(m.(out));
        if c <> out then m.(c) := []
      done;
      m.(out) := List.sort (fun (t1, _) (t2, _) -> t1 - t2) !(m.(out))
end

class remove (source:source) t =
object (self)
  inherit base (source)

  method private get_frame buf =
    source#get buf;
    let m = MFrame.tracks buf in
      List.iter (fun c -> m.(c) := []) t
end

let () =
  Lang.add_operator "midi.merge_all"
    [
      "track_out", Lang.int_t, Some (Lang.int 0), Some "Destination track.";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.MIDIProcessing
    ~descr:"Merge all MIDI tracks in one."
    (fun p _ ->
       let f v = List.assoc v p in
       let out = Lang.to_int (f "track_out") in
       let src = Lang.to_source (f "") in
         new merge src out)

let () =
  Lang.add_operator "midi.remove"
    [
      "", Lang.list_t Lang.int_t, None, Some "Tracks to remove.";
      "", Lang.source_t, None, None
    ]
    ~category:Lang.MIDIProcessing
    ~descr:"Remove MIDI tracks."
    (fun p _ ->
       (* let f v = List.assoc v p in *)
       let t = List.map Lang.to_int (Lang.to_list (Lang.assoc "" 1 p)) in
       let src = Lang.to_source (Lang.assoc "" 2 p) in
         new remove src t)
