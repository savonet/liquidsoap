(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

class map_metadata source rewrite_f insert_missing update strip =
object (self)
  inherit operator [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  method rewrite m =
    let m' = Lang.apply rewrite_f ["",Lang.metadata m] in
    let replace_val v =
      let (x,y) = Lang.to_product v in
      let x = Lang.to_string x and y = Lang.to_string y in
      if not strip then
        Hashtbl.replace m x y
      else
        if y <> "" then
          Hashtbl.replace m x y
	else
          Hashtbl.remove m x
    in
    if not update then Hashtbl.clear m ;
    List.iter replace_val (Lang.to_list m')

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
           if t>=p then begin
             self#rewrite m ;
             if strip && Hashtbl.length m = 0 then
               Frame.free_metadata buf t
           end)
        (Frame.get_all_metadata buf)

end

let register =
  Lang.add_operator "map_metadata"
    [ "", Lang.fun_t
            [false,"",
             Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)]
            (Lang.list_t (Lang.product_t Lang.string_t Lang.string_t)),
      None, Some "A function that returns new metadata." ;
      "update", Lang.bool_t, Some (Lang.bool true),
      Some "Only update metadata. If false, only returned values \
      will be set as metadata." ;
      "strip", Lang.bool_t, Some (Lang.bool false),
      Some "Completly remove empty metadata. Operates on both empty values \
      and empty metadata chunk.";
      "insert_missing", Lang.bool_t, Some (Lang.bool true),
      Some "Treat track beginnings without metadata as having empty ones. \
      The operational order is: create empty if needed, map and strip if enabled." ;
      "", Lang.source_t, None, None ]
    ~category:Lang.TrackProcessing
    ~descr:"Rewrite metadata on the fly using a function."
    (fun p ->
       let source = Lang.to_source (Lang.assoc "" 2 p) in
       let f = Lang.assoc "" 1 p in
       let update = Lang.to_bool (List.assoc "update" p) in
       let strip = Lang.to_bool (List.assoc "strip" p) in
       let missing = Lang.to_bool (List.assoc "insert_missing" p) in
         ((new map_metadata source f missing update strip):>source))
