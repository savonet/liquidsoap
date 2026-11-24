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

(** A format parameter that defaults to a frame setting holds the setting, not
    its value, until start-up. *)

type setting = [ `Audio_rate | `Video_width | `Video_height ]
type t = [ `Setting of setting | `Value of int ]

let setting_name = function
  | `Audio_rate -> "settings.frame.audio.samplerate"
  | `Video_width -> "settings.frame.video.width"
  | `Video_height -> "settings.frame.video.height"

let default = function
  | `Audio_rate -> 44100
  | `Video_width -> Video_format.default_width
  | `Video_height -> Video_format.default_height

(** The setting's current value, leaving it free to change. Frame settings
    implement it; a process without them reads the defaults. *)
let read_setting : (setting -> int) ref = ref default

(** The setting's value, fixed from then on. *)
let fix_setting : (setting -> int) ref = ref default

let peek = function `Value v -> v | `Setting s -> !read_setting s
let force = function `Value v -> v | `Setting s -> !fix_setting s
