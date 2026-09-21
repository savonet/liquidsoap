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

(** Operations on frames, which are small portions of streams. *)

(** {2 Frame definitions} *)

module Fields = Fields

type field = Fields.field

(** Precise description of the channel types for the current track. *)
type content_type = Content_base.format Fields.t

type t = Content_base.data Fields.t

let position frame =
  Option.value ~default:0
    (Fields.fold
       (fun _ c -> function
         | None -> Some (Content_base.length c)
         | Some p -> Some (Int.min p (Content_base.length c)))
       frame None)

let remaining b = Lazy.Mutexed.force Frame_settings.size - position b
let is_partial b = 0 < remaining b

(** Metadata of a frame. *)
module Metadata = Metadata_base

type metadata = Metadata_base.t

let audio_format = Pcm_format.audio_format
let format_of_channels = Pcm_format.format_of_channels

let add_timed_content ?length content =
  Fields.add Fields.track_marks
    (Content_base.make ?length Content_timed.Track_marks.format)
    (Fields.add Fields.metadata
       (Content_base.make ?length Content_timed.Metadata.format)
       content)
