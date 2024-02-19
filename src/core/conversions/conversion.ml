(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2024 Savonet team

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

(* Conversion base class. contents marked as [true] are modified
 * by the converter using this class. Other contents are untouched. *)
class base ?(audio = false) ?(video = false) ?(midi = false) ~converter
  (source : Source.source) =
  object (self)
    method stype = source#stype
    method private _is_ready = source#is_ready
    method abort_track = source#abort_track
    method remaining = source#remaining
    method seek_source = source#seek_source
    method self_sync = source#self_sync
    val mutable tmp_frame = None

    (* The tmp_frame is intended to be filled by the underlying source. Content
       untouched by the converter are replaced by by content from the calling
       frame. Touched content get their own layer. *)
    method private tmp_frame =
      match tmp_frame with
        | Some tmp_frame -> tmp_frame
        | None ->
            (* We need to delay the creation of the frame in order not to evaluate
               the kind too early (the user has to have a chance of setting the
               default number of channels). *)
            let frame = Frame.create source#content_type in
            tmp_frame <- Some frame;
            frame

    method private copy_frame src dst =
      Frame.set_breaks dst (Frame.breaks src);
      Frame.set_all_metadata dst (Frame.get_all_metadata src);
      if not audio then (
        try Frame.set_audio dst (Frame.audio src) with Not_found -> ());
      if not video then (
        try Frame.set_video dst (Frame.video src) with Not_found -> ());
      if not midi then (
        try Frame.set_video dst (Frame.midi src) with Not_found -> ())

    method private get_frame frame =
      let tmp_frame = self#tmp_frame in
      self#copy_frame frame tmp_frame;
      source#get tmp_frame;
      converter ~frame tmp_frame;
      self#copy_frame tmp_frame frame
  end
