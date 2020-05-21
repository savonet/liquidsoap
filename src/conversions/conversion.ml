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

(* Conversion base class. contents marked as [true] are modified
 * by the converter using this class. Other contents are untouched. *)
class base ?(audio = false) ?(video = false) ?(midi = false) ~converter
  (source : Source.source) =
  object (self)
    method stype = source#stype

    method is_ready = source#is_ready

    method abort_track = source#abort_track

    method remaining = source#remaining

    method seek = source#seek

    method self_sync = source#self_sync

    (* The tmp_frame is intended to be filled by the underlying
     * source. Content untouched by the converter are replaced by
     * by content from the calling frame. Touched content get their
     * own layer. *)
    val mutable tmp_frame = Frame.create source#kind

    method private tmp_frame = tmp_frame

    method private copy_frame src dst =
      Frame.set_pts dst (Frame.pts src);
      Frame.set_breaks dst (Frame.breaks src);
      Frame.set_all_metadata dst (Frame.get_all_metadata src);
      if not audio then Frame.set_audio dst (Frame.audio src);
      if not video then Frame.set_video dst (Frame.video src);
      if not midi then Frame.set_midi dst (Frame.midi src)

    method private get_frame frame =
      let tmp_frame = self#tmp_frame in
      self#copy_frame frame tmp_frame;
      source#get tmp_frame;
      converter ~frame tmp_frame;
      self#copy_frame tmp_frame frame
  end
