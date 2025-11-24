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

class midimeter source =
  object
    inherit operator ~name:"midi.inspect" [source]
    method fallible = source#fallible
    method private can_generate_frame = source#is_ready
    method remaining = source#remaining
    method abort_track = source#abort_track
    method self_sync = source#self_sync
    method effective_source = source#effective_source

    method generate_frame =
      let m =
        Content.Midi.get_data (source#get_mutable_content Frame.Fields.midi)
      in
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
                | _ -> "???"
            in
            Printf.printf "%d: %s.\n%!" c s)
          (MIDI.data m.(c))
      done;
      source#set_frame_data Frame.Fields.midi Content.Midi.lift_data m
  end

let _ =
  let frame_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator ~base:Modules.midi "inspect"
    [("", Lang.source_t frame_t, None, None)]
    ~return_t:frame_t ~category:`Visualization
    ~descr:"Display midi events on standard output."
    (fun p ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      (new midimeter src :> Source.source))
