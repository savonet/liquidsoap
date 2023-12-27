(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

exception Invalid_override of string

let ( -- ) = Int64.sub
let ( ++ ) = Int64.add

let ticks_of_offset offset =
  Int64.of_float (offset *. float (Lazy.force Frame.main_rate))

class on_offset ~force ~offset f s =
  object (self)
    inherit Source.operator ~name:"on_offset" [s]
    inherit Latest_metadata.source
    method stype = s#stype
    method private can_generate_frame = s#is_ready
    method remaining = s#remaining
    method abort_track = s#abort_track
    method seek_source = s#seek_source
    method self_sync = s#self_sync
    val mutable elapsed = 0L
    method offset = ticks_of_offset (offset ())
    val mutable executed = false

    method private execute =
      self#log#info "Executing on_offset callback.";
      let pos = Int64.to_float elapsed /. float (Lazy.force Frame.main_rate) in
      ignore
        (Lang.apply f
           [("", Lang.float pos); ("", Lang.metadata latest_metadata)]);
      executed <- true

    method private on_new_metadata = ()

    method private on_frame buf =
      self#save_latest_metadata buf;
      let new_pos = Int64.of_int (Frame.position buf) in
      elapsed <- elapsed ++ new_pos;
      if (not executed) && self#offset <= elapsed then self#execute

    method private generate_frame =
      let buf = s#get_frame in
      match self#split_frame buf with
        | frame, None ->
            self#on_frame frame;
            frame
        | frame, Some new_frame ->
            self#on_frame frame;
            if force && not executed then self#execute;
            executed <- false;
            self#clear_latest_metadata;
            elapsed <- Int64.of_int (Frame.position new_frame);
            buf
  end

let _ =
  let return_t = Lang.frame_t (Lang.univ_t ()) Frame.Fields.empty in
  Lang.add_operator "on_offset"
    [
      ( "offset",
        Lang.getter_t Lang.float_t,
        None,
        Some
          "Execute handler when position in track is equal or more than to \
           this value." );
      ( "force",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Force execution of callback if track ends before 'offset' position \
           has been reached." );
      ( "",
        Lang.fun_t
          [(false, "", Lang.float_t); (false, "", Lang.metadata_t)]
          Lang.unit_t,
        None,
        Some
          "Function to execute. First argument is the actual position within \
           the current track, second is the latest metadata. That function \
           should be fast because it is executed in the main streaming thread."
      );
      ("", Lang.source_t return_t, None, None);
    ]
    ~category:`Track
    ~descr:
      "Call a given handler when position in track is equal or more than a \
       given amount of time."
    ~return_t
    (fun p ->
      let offset = List.assoc "offset" p |> Lang.to_float_getter in
      let force = List.assoc "force" p |> Lang.to_bool in
      let f = Lang.assoc "" 1 p in
      let s = Lang.to_source (Lang.assoc "" 2 p) in
      new on_offset ~offset ~force f s)
