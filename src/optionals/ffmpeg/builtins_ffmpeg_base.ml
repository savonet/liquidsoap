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

let ffmpeg = Lang.add_module "ffmpeg"
let ffmpeg_filter = Lang.add_module ~base:ffmpeg "filter"
let ffmpeg_raw = Lang.add_module ~base:ffmpeg "raw"
let track_ffmpeg = Lang.add_module ~base:Modules.track "ffmpeg"
let track_ffmpeg_raw = Lang.add_module ~base:track_ffmpeg "raw"
