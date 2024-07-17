(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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

class rewrite_metadata source patterns insert_missing =
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

  val mutable in_track = false

  method get_frame buf =
    let p = Frame.position buf in
      source#get buf ;
      if insert_missing && not in_track && Frame.position buf > p then begin
        in_track <- true ;
        match Frame.get_metadata buf p with
          | None ->
              self#log#f 3 "Inserting missing metadata." ;
              let h = Hashtbl.create 10 in
                Frame.set_metadata buf p h
          | Some _ -> ()
      end ;
      if Frame.is_partial buf then in_track <- false ;
      List.iter
        (fun (t,m) ->
           if t>=p then
             self#rewrite m)
        (Frame.get_all_metadata buf)

end

let register =
  Lang.add_operator "rewrite_metadata"
    [ "", Lang.list_t (Lang.product_t Lang.string_t Lang.string_t),
      None, Some "List of (target,value) rewriting rules." ;
      "insert_missing", Lang.bool_t, Some (Lang.bool true),
      Some "Treat track beginnings without metadata as having empty ones." ;
      "", Lang.source_t, None, None ]
    ~category:Lang.TrackProcessing
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
       let missing = Lang.to_bool (List.assoc "insert_missing" p) in
         ((new rewrite_metadata source l missing):>source))
