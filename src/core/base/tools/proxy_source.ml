(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

type t = { source : Source.source; is_used : unit -> bool }

class proxy (s : Source.source) =
  object
    inherit Source.operator ~name:"proxy_source" [s]
    method fallible = s#fallible
    method remaining = s#remaining
    method abort_track = s#abort_track
    method effective_source = s#effective_source
    method self_sync = s#self_sync
    method private can_generate_frame = s#is_ready
    method private generate_frame = s#get_frame
  end

let create_proxy s =
  let proxy = new proxy s in
  let w = Weak.create 1 in
  Weak.set w 0 (Some proxy);
  let is_used () = Weak.check w 0 in
  { source = (proxy :> Source.source); is_used }
