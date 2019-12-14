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

class midimeter ~kind source =
  object
    inherit operator ~name:"midimeter" kind [source]

    method stype = source#stype

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method self_sync = source#self_sync

    method get_frame buf =
      let offset = MFrame.position buf in
      source#get buf ;
      let m = MFrame.content buf offset in
      (* Printf.printf "len: %d\n%!" (List.length (MIDI.data m.(0))); *)
      for c = 0 to Array.length m - 1 do
        List.iter
          (fun (_, e) ->
            let s =
              match e with
                | MIDI.Note_on (n, v) ->
                    Printf.sprintf "Note %d on at %.02f" n v
                | MIDI.Note_off (n, v) ->
                    Printf.sprintf "Note %d off at %.02f" n v
                | MIDI.Control_change (c, v) ->
                    Printf.sprintf "Control %x at %d" c v
                | _ ->
                    "???"
            in
            Printf.printf "%d: %s.\n%!" c s)
          (MIDI.data m.(c))
      done
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any_fixed in
  Lang.add_operator "midimeter"
    [("", Lang.source_t k, None, None)]
    ~kind:(Lang.Unconstrained k) ~category:Lang.Visualization
    ~flags:[Lang.Hidden; Lang.Experimental]
    ~descr:"Display midi events."
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      (new midimeter ~kind src :> Source.source))
