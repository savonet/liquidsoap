(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

(* Reads data from an audio buffer generator.
   A thread safe generator should be used if it has to be fed concurrently.
   Store [bufferize] seconds before declaring itself as ready. *)
class virtual source ?(seek = false) ?(replay_meta = false) ~bufferize
  ~empty_on_abort () =
  let bufferize = Frame.main_of_seconds bufferize in
  object (self : < Source.source ; .. >)
    val mutable buffering = true
    val mutable add_track_mark = false
    val mutable cur_meta : Frame.metadata option = None
    method virtual private log : Log.t
    method virtual private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b
    method virtual buffer : Generator.t
    method self_sync : Clock.self_sync = (`Static, None)

    method seek len =
      if (not seek) || len <= 0 then 0
      else
        self#mutexify
          (fun () ->
            let len = min len (Generator.remaining self#buffer) in
            Generator.truncate self#buffer len;
            len)
          ()

    method effective_source = (self :> Source.source)
    method abort_track = add_track_mark <- true
    method private length = Generator.length self#buffer
    val mutable last_buffering_warning = -1

    method private can_generate_frame =
      let r = self#length in
      if buffering then (
        (* We have some data, but not enough for safely starting to play it. *)
        if bufferize > 0 && r <= bufferize && r <> last_buffering_warning then (
          last_buffering_warning <- r;
          self#log#debug "Not ready: need more buffering (%i/%i)." r bufferize);
        r > bufferize)
      else r > 0

    method remaining =
      if add_track_mark then 0
      else if buffering && self#length <= bufferize then 0
      else Generator.remaining self#buffer

    (* Returns true if metadata should be replayed. *)
    method private save_metadata frame =
      let new_meta =
        match
          List.fold_left
            (function
              | None -> fun (p, m) -> Some (p, m)
              | Some (curp, curm) ->
                  fun (p, m) ->
                    Some (if p >= curp then (p, m) else (curp, curm)))
            (match cur_meta with None -> None | Some m -> Some (-1, m))
            (Frame.get_all_metadata frame)
        with
          | None -> None
          | Some (_, m) -> Some m
      in
      if cur_meta = new_meta then true
      else (
        cur_meta <- new_meta;
        false)

    method private replay_metadata frame =
      match cur_meta with
        | None -> frame
        | Some m -> Frame.add_metadata frame 0 m

    method private generate_frame =
      self#mutexify
        (fun () ->
          let was_buffering = buffering in
          buffering <- false;
          if add_track_mark && empty_on_abort then Generator.clear self#buffer;
          let buf = Generator.slice self#buffer (Lazy.force Frame.size) in
          let buf =
            if was_buffering || add_track_mark then (
              self#log#info "Adding track mark.";
              add_track_mark <- false;
              Frame.add_track_mark buf 0)
            else buf
          in
          if Generator.length self#buffer = 0 then (
            self#log#info "Buffer emptied, buffering needed.";
            buffering <- true);
          if self#save_metadata buf && was_buffering && replay_meta then
            self#replay_metadata buf
          else buf)
        ()
  end
