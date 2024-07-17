(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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

class rewrite_metadata source patterns =
object (self)
  inherit operator [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method rewrite m =
    List.iter
      (fun (t,v) -> Hashtbl.replace m t v)
      (List.map (fun (t,v) -> t, Utils.interpolate (Hashtbl.find m) v) patterns)

  method get_frame buf =
    let p = Mixer.Buffer.position buf in
      source#get buf ;
      List.iter
        (fun (t,m) ->
           if t>=p then
             self#rewrite m)
        (Mixer.Buffer.get_all_metadata buf)
end

let register =
  Lang.add_operator "rewrite_metadata"
    [ "", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
      None, Some "List of (target,value) rewriting rules." ;
      "", Lang.source_t, None, None ]
    ~descr:"Rewrite metadata on the fly."
    (fun p ->
       let source = Lang.to_source (Lang.assoc "" 2 p) in
       let l =
         List.map
           (fun x ->
              let (a,b) = Lang.to_product x in
                Lang.to_string a, Lang.to_string b)
           (Lang.to_list (Lang.assoc "" 1 p))
       in
         ((new rewrite_metadata source l):>source))
