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

class available ~kind ~roll ~on source =
object (self)
  inherit operator ~name:"available" kind [source]

  initializer
    ns_kind <- "available" ;
    let status _ = string_of_bool (on ()) in
    self#register_command "is_available" ~descr:"Check if the source is available." status

  method stype = Fallible
  method is_ready = on () && source#is_ready
  method abort_track = source#abort_track
  method remaining = source#remaining

  method private get_frame buf =
    if on () then
      source#get buf
    else
      if roll () then
        let b = Frame.breaks buf in
        let p = Frame.position buf in
        source#get buf;
        Frame.set_breaks buf (p::b)
      else
        let p = Frame.position buf in
        Frame.add_break buf p
end

let () =
  let kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "available"
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.TrackProcessing
    ~descr:"Make a source available or not depending on a boolean."
    [
      "roll", Lang.bool_getter_t 2, Some (Lang.bool false), Some "Whether the source should continue to roll when unavailable";
      "", Lang.bool_getter_t 3, None, Some "Whether the source is available (`true`) or not (`false`).";
      "", Lang.source_t kind, None, None
    ]
    (fun p kind ->
      let roll = Lang.to_bool_getter (List.assoc "roll" p) in
      let on = Lang.to_bool_getter (Lang.assoc "" 1 p) in
      let s = Lang.to_source (Lang.assoc "" 2 p) in
      ((new available ~kind ~roll ~on s):>Source.source))
