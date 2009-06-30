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

class midimeter source =
object
  inherit operator [source] as super

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method get_frame buf =
    source#get buf;
    let m = MFrame.tracks buf in
      for c = 0 to Array.length m - 1 do
        List.iter
          (fun (t, e) ->
             let s =
               match e with
                 | Midi.Note_on (n, v) ->
                     Printf.sprintf "Note %d on at %.02f" n v
                 | Midi.Note_off (n, v) ->
                     Printf.sprintf "Note %d off at %.02f" n v
                 | _ -> "???"
             in
               Printf.printf "%d: %s.\n%!" c s
          ) !(m.(c))
      done
end

let () =
  Lang.add_operator "midimeter"
    [ "", Lang.source_t, None, None ]
    ~category:Lang.Visualization
    ~flags:[Lang.Hidden; Lang.Experimental]
    ~descr:"Display midi events."
    (fun p _ ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         ((new midimeter src):>Source.source))
