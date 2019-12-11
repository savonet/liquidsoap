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

class id ~kind ?(name = "id") (source : source) =
  object
    inherit operator ~name kind [source]

    method stype = source#stype

    method remaining = source#remaining

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame buf = source#get buf
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "id"
    [("", Lang.source_t kind, None, None)]
    ~category:Lang.Conversions
    ~descr:"Does not do anything, simply forwards its input stream."
    ~kind:(Lang.Unconstrained kind)
    (fun p kind ->
      let f v = List.assoc v p in
      let src = Lang.to_source (f "") in
      new id ~kind src)
