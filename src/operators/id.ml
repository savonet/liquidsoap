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

class id (source:source) =
object (self)
  inherit operator [source] as super

  method stype = source#stype

  method remaining = source#remaining

  method is_ready = source#is_ready

  method abort_track = source#abort_track

  method get_frame buf = source#get buf
end

let () =
  Lang.add_operator "id"
    ["", Lang.source_t, None, None]
    ~category:Lang.SoundProcessing
    ~descr:"Identity: does not do anything."
    ~flags:[Lang.Hidden]
    (fun p ->
       let f v = List.assoc v p in
       let src = Lang.to_source (f "") in
         ((new id src):>source)
    )
