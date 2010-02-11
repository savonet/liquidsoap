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

let chan = 0

class chord ~kind (source:source) =
object (self)
  inherit operator kind [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method private get_frame buf =
    let offset = MFrame.position buf in
    source#get buf;
    let m = MFrame.content buf offset in
    (* let meta = MFrame.get_all_metadata buf in *)
    let notes_on = ref [] in
    let chords = [0,72,""] in
    let play t n =
      let notes = List.map (fun n -> t, Midi.Note_on (n,1.)) n in
        m.(chan) := notes @ !(m.(chan));
        notes_on := n @ !notes_on
    in
    let mute t =
      let notes = List.map (fun n -> t, Midi.Note_off (n,1.)) !notes_on in
        m.(chan) := notes @ !(m.(chan));
        notes_on := []
    in
      List.iter
        (fun (t,c,m) -> (* time, base, mode *)
           (* Negative base note means mute. *)
           if c >= 0 then
             (
               match m with
                 | "" | "M" -> (* major *)
                     play t [c;c+4;c+7]
                 | "m" -> (* minor *)
                     play t [c;c+3;c+7]
                 | "7" ->
                     play t [c;c+4;c+7;c+10]
                 | "M7" ->
                     play t [c;c+4;c+7;c+11]
                 | "m7" ->
                     play t [c;c+3;c+7;c+10]
                 | "dim" ->
                     play t [c;c+3;c+6]
                 | m ->
                     self#log#f 5 "Unknown mode: %s\n%!" m
             );
           (* It's important to call this after because we want to mute before
            * playing the new chord. *)
           mute t
        ) chords;
      m.(chan) := Mutils.sort_track !(m.(chan))
end

let () =
  let k = Lang.kind_type_of_kind_format ~fresh:1 (Lang.any_fixed_with ~midi:1 ()) in
  Lang.add_operator "midi.chord"
    [
      "", Lang.source_t k, None, None
    ]
    ~kind:(Lang.Unconstrained k)
    ~category:Lang.MIDIProcessing
    ~descr:"Generate a chord."
    (fun p kind ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         new chord ~kind src)
