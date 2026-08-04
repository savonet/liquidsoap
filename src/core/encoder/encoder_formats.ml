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

(* These live in Metadata_settings: the request resolver and the metadata
   export code read them from below the encoders. Re-exported here under their
   historical names. *)
let conf = Metadata_settings.conf
let conf_meta = Metadata_settings.conf_meta
let conf_meta_cover = Metadata_settings.conf_meta_cover
let conf_export_metadata = Metadata_settings.conf_export_metadata
let string_of_stereo s = if s then "stereo" else "mono"
