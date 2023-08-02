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

(* Reads data from an audio buffer generator.
 * A thread safe generator should be used if it has to be fed concurrently.
 * Store [bufferize] seconds before declaring itself as ready. *)
class virtual source ?(seek = false) ?(replay_meta = false) ~bufferize
  ~empty_on_abort () =
  let bufferize = Frame.main_of_seconds bufferize in
  object (self : < Source.source ; .. > as 'a)
    val mutable buffering = true
    val mutable should_fail = false
    val mutable cur_meta : Request.metadata option = None
    method virtual private log : Log.t
    method virtual private mutexify : 'a 'b. ('a -> 'b) -> 'a -> 'b
    method virtual buffer : Generator.t
    method self_sync : Source.self_sync = (`Static, false)

    method seek len =
      if (not seek) || len <= 0 then 0
      else
        self#mutexify
          (fun () ->
            let len = min len (Generator.remaining self#buffer) in
            Generator.truncate self#buffer len;
            len)
          ()

    method seek_source = (self :> Source.source)
    method abort_track = should_fail <- true
    method private length = Generator.length self#buffer
    val mutable last_buffering_warning = -1

    method is_ready =
      let r = self#length in
      if buffering then (
        (* We have some data, but not enough for safely starting to play it. *)
        if bufferize > 0 && r <= bufferize && r <> last_buffering_warning then (
          last_buffering_warning <- r;
          self#log#debug "Not ready: need more buffering (%i/%i)." r bufferize);
        r > bufferize)
      else (
        (* This only happens if the end of track has not been played yet,
         * after which the buffering phase will start again. Does not mean
         * that we're not accumulating data, but it means that we don't know
         * yet that we'll stop playing it until the buffer is full enough. *)
        if r = 0 then self#log#info "Not ready for a new track: empty buffer.";
        r > 0)

    method remaining =
      if should_fail then 0
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

    method private replay_metadata pos frame =
      match cur_meta with
        | None -> ()
        | Some m -> Frame.set_metadata frame pos m

    method private get_frame ab =
      self#mutexify
        (fun () ->
          let was_buffering = buffering in
          let pos = Frame.position ab in
          buffering <- false;
          if should_fail then (
            self#log#info "Performing skip.";
            should_fail <- false;
            if empty_on_abort then Generator.clear self#buffer;
            Frame.add_break ab (Frame.position ab))
          else (
            Generator.fill self#buffer ab;

            (* Currently, we don't enter the buffering phase between tracks
             * even when there's not enough data in the buffer. This is mostly
             * historical because there was initially no breaks in generators.
             * This may sometimes be better to do it (to avoid a lag breaking
             * the new track) but not always (a total disconnection should cause
             * the start of a new track anyway, since the content after it
             * has nothing to do with the content before the connection). *)
            if Frame.is_partial ab then self#log#info "End of track.";
            if Generator.length self#buffer = 0 then (
              self#log#info "Buffer emptied, buffering needed.";
              buffering <- true);
            if self#save_metadata ab && was_buffering && replay_meta then
              self#replay_metadata pos ab))
        ()
  end

(* Reads data from a fixed buffer generator. The generator shouldn't be fed anymore. *)
class consumer buffer =
  object
    inherit Source.source ~name:"buffer" ()
    inherit source ~bufferize:0. ~empty_on_abort:true ()
    method stype = `Fallible
    method! buffer = buffer
  end
