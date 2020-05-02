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

class infallible ~kind (source : source) =
  object
    inherit operator ~name:"source.infallible" kind [source]

    method stype = Infallible

    method is_ready = source#is_ready

    method remaining = source#remaining

    method abort_track = source#abort_track

    method seek = source#seek

    method self_sync = source#self_sync

    method private get_frame buf = source#get buf
  end

let () =
  let k = Lang.kind_type_of_kind_format Lang.any in
  Lang.add_operator "source.infallible"
    [("", Lang.source_t k, None, None)]
    ~return_t:k ~category:Lang.Liquidsoap
    ~descr:
      "Assert that a source is infallible. This should be used with care since \
       this disables fallibility checks for the source, and is mostly reserved \
       for use in the standard library."
    ~flags:[Lang.Hidden]
    (fun p kind ->
      let s = Lang.to_source (List.assoc "" p) in
      new infallible ~kind s)
