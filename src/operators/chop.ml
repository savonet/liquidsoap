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
exception Error

class chop ~kind ~duration source =
object
  inherit operator ~name:"chop" kind [source]

  method stype = source#stype
  method is_ready = source#is_ready
  method remaining = source#remaining
  method abort_track = source#abort_track

  val mutable remaining =
    Frame.master_of_seconds (duration ())

  method private get_frame buf =
    let p = Frame.position buf in
    (* TODO: is there an easy way to be sub-frame precise? *)
    if remaining <=0 then
      (
        Frame.add_break buf p;
        remaining <- remaining + Frame.master_of_seconds (duration ())
      )
    else
      source#get buf;
    let q = Frame.position buf in
    remaining <- remaining - (q - p)
end

let () =
  let kind = Lang.kind_type_of_kind_format ~fresh:1 Lang.any_fixed in
  Lang.add_operator "chop"
    ~category:Lang.TrackProcessing
    ~kind:(Lang.Unconstrained kind)
    ~descr:"Regularly insert track boundaries in a stream (useful for testing tracks)."
    [
      "duration", Lang.float_getter_t 2, Some (Lang.float 3.), Some "Duration of a track (in seconds).";
      "", Lang.source_t kind, None, None
    ]
    (fun p kind ->
      let s = Lang.to_source (List.assoc "" p) in
      let duration = Lang.to_float_getter (List.assoc "duration" p) in
      new chop ~kind ~duration s)
