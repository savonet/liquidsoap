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

module Make (Generator : Generator.S) = struct
  (* Reads data from an audio buffer generator.
   * A thread safe generator should be used if it has to be fed concurrently.
   * Store [bufferize] seconds before declaring itself as ready. *)
  class virtual source ?(seek = false) ?(replay_meta = false) ~bufferize
    ~empty_on_abort gen =
    let bufferize = Frame.master_of_seconds bufferize in
    object (self)
      (** We keep the generator in an instance variable so that derived
    * classes can access it. There are concurrency issues, though:
    *  - In classes such as http_source, the generator is fed concurrently
    *    with its consumption. But in that case a thread-safe implementation
    *    of Generator is used.
    *  - In any case there is always concurrency in this class, with #get
    *    on one hand and #is_ready, #abort_track and #seek that can be
    *    called from other threads. We use the generator_lock to avoid
    *    the only bad interference, ie. #get_frame vs #seek. *)
      val generator = gen

      val generator_lock = Mutex.create ()

      val mutable buffering = true

      val mutable should_fail = false

      val mutable cur_meta : Request.metadata option = None

      method virtual private log : Log.t

      method self_sync = false

      method seek len =
        if (not seek) || len <= 0 then 0
        else
          Tutils.mutexify generator_lock
            (fun () ->
              let len = min len (Generator.remaining generator) in
              Generator.remove generator len;
              len)
            ()

      method abort_track = should_fail <- true

      method private length = Generator.length generator

      val mutable last_buffering_warning = -1

      method is_ready =
        let r = self#length in
        if buffering then (
          (* We have some data, but not enough for safely starting to play it. *)
          if bufferize > 0 && r <= bufferize && r <> last_buffering_warning then (
            last_buffering_warning <- r;
            self#log#debug "Not ready: need more buffering (%i/%i)." r bufferize
            );
          r > bufferize )
        else (
          (* This only happens if the end of track has not been played yet,
           * after which the buffering phase will start again. Does not mean
           * that we're not accumulating data, but it means that we don't know
           * yet that we'll stop playing it until the buffer is full enough. *)
          if r = 0 then self#log#info "Not ready for a new track: empty buffer.";
          r > 0 )

      method remaining =
        if should_fail then 0
        else if buffering && self#length <= bufferize then 0
        else Generator.remaining generator

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
          false )

      method private replay_metadata pos frame =
        match cur_meta with
          | None -> ()
          | Some m -> Frame.set_metadata frame pos m

      method private get_frame ab =
        Tutils.mutexify generator_lock
          (fun () ->
            let was_buffering = buffering in
            let pos = Frame.position ab in
            buffering <- false;
            if should_fail then (
              self#log#info "Performing skip.";
              should_fail <- false;
              if empty_on_abort then Generator.clear generator;
              Frame.add_break ab (Frame.position ab) )
            else (
              Generator.fill generator ab;
              (* Currently, we don't enter the buffering phase between tracks
               * even when there's not enough data in the buffer. This is mostly
               * historical because there was initially no breaks in generators.
               * This may sometimes be better to do it (to avoid a lag breaking
               * the new track) but not always (a total disconnection should cause
               * the start of a new track anyway, since the content after it
               * has nothing to do with the content before the connection). *)
              if Frame.is_partial ab then self#log#info "End of track.";
              if Generator.length generator = 0 then (
                self#log#info "Buffer emptied, buffering needed.";
                buffering <- true );
              if self#save_metadata ab && was_buffering && replay_meta then
                self#replay_metadata pos ab ))
          ()
    end

  (* Reads data from a fixed audio buffer generator, of a certain kind.
   * The generator shouldn't be fed anymore. *)
  class consumer ~kind generator =
    object
      inherit Source.source ~name:"buffer" kind

      inherit source generator ~bufferize:0. ~empty_on_abort:true

      method stype = Source.Fallible
    end
end

module From_audio_video_plus = Make (Generator.From_audio_video_plus)
