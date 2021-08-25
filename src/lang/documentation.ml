(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

(** Documentation flags. *)
type flag = [ `Hidden | `Deprecated | `Experimental | `Extra ]

let string_of_flag : flag -> string = function
  | `Hidden -> "hidden"
  | `Deprecated -> "deprecated"
  | `Experimental -> "experimental"
  | `Extra -> "extra"

(** Kind of source. *)
type source =
  [ `Input
  | `Output
  | `Conversion
  | `FFmpegFilter
  | `Track
  | `Audio
  | `Video
  | `MIDI
  | `Visualization
  | `Synthesis
  | `Liquidsoap ]

let string_of_source : source -> string = function
  | `Input -> "Input"
  | `Output -> "Output"
  | `Conversion -> "Conversion"
  | `FFmpegFilter -> "FFmpeg Filter"
  | `Track -> "Track Processing"
  | `Audio -> "Audio Processing"
  | `Video -> "Video Processing"
  | `MIDI -> "MIDI Processing"
  | `Synthesis -> "Sound Synthesis"
  | `Visualization -> "Visualization"
  | `Liquidsoap -> "Liquidsoap"

type category =
  [ `Source of source
  | `System
  | `File
  | `Math
  | `String
  | `List
  | `Bool
  | `Liquidsoap
  | `Control
  | `Interaction
  | `Other
  | `Filter ]

let string_of_category : category -> string = function
  | `Source s -> "Source / " ^ string_of_source s
  | `System -> "System"
  | `File -> "File"
  | `Math -> "Math"
  | `String -> "String"
  | `List -> "List"
  | `Bool -> "Bool"
  | `Liquidsoap -> "Liquidsoap"
  | `Control -> "Control"
  | `Interaction -> "Interaction"
  | `Other -> "Other"
  | `Filter -> "Filter"
