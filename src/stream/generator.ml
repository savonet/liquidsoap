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

exception Incorrect_stream_type

module type S = sig
  type t

  val length : t -> int (* ticks *)

  val remaining : t -> int (* ticks *)

  val clear : t -> unit

  val fill : t -> Frame.t -> unit

  val remove : t -> int -> unit

  val add_metadata : t -> Frame.metadata -> unit
end

module type S_Asio = sig
  type t

  val length : t -> int (* ticks *)

  val audio_length : t -> int

  val video_length : t -> int

  val remaining : t -> int (* ticks *)

  val clear : t -> unit

  val fill : t -> Frame.t -> unit

  val add_metadata : t -> Frame.metadata -> unit

  val add_break : ?sync:[`Strict | `Ignore | `Drop] -> t -> unit

  val put_audio : t -> Frame.audio_t array -> int -> int -> unit

  val put_video : t -> Frame.video_t array -> int -> int -> unit

  val set_mode : t -> [`Audio | `Video | `Both | `Undefined] -> unit
end

(** The base module doesn't even know what kind of data it is buffering. *)
module Generator = struct
  (** A chunk with given offset and length. *)
  type 'a chunk = 'a * int * int

  let chunk_data ((buf, _, _) : 'a chunk) = buf

  (** A buffer is a queue of chunks. *)
  type 'a buffer = 'a chunk Queue.t

  (** All positions and lengths are in ticks. *)
  type 'a t = {
    mutable length: int;
    mutable offset: int;
    mutable buffers: 'a buffer;
  }

  let create () = {length= 0; offset= 0; buffers= Queue.create ()}

  let clear g =
    g.length <- 0 ;
    g.offset <- 0 ;
    g.buffers <- Queue.create ()

  let length b = b.length

  (** Remove [len] ticks of data. *)
  let rec remove g len =
    assert (g.length >= len) ;
    if len > 0 then (
      let _, _, b_len = Queue.peek g.buffers in
      (* Is it enough to advance in the first buffer?
       * Or do we need to consume it completely and go farther in the queue? *)
      if g.offset + len < b_len then (
        g.length <- g.length - len ;
        g.offset <- g.offset + len )
      else (
        let removed = b_len - g.offset in
        ignore (Queue.take g.buffers) ;
        g.length <- g.length - removed ;
        g.offset <- 0 ;
        remove g (len - removed) ) )

  (** Remove data at the end of the generator: this is not a natural operation
  * for Generators, it's done in linear time. *)
  let remove_end g remove_len =
    (* Remove length [l] at the beginning of the buffers,
     * should correspond exactly to some last [n] chunks. *)
    let rec remove l =
      if l > 0 then (
        let _, _, len = Queue.take g.buffers in
        assert (l >= len) ;
        remove (l - len) )
    in
    (* Go through the beginning of length [l] of the queue,
     * possibly cut some element in half, remove the rest.
     * The parsed elements are put back at the end
     * of the queue. *)
    let rec cut l =
      let c, ofs, len = Queue.take g.buffers in
      if len < l then (
        Queue.push (c, ofs, len) g.buffers ;
        cut (l - len) )
      else (
        Queue.push (c, ofs, l) g.buffers ;
        remove (remove_len - (len - l)) )
    in
    let new_len = g.length - remove_len in
    assert (remove_len > 0 && new_len >= 0) ;
    cut new_len ;
    g.length <- new_len

  (** Feed an item into a generator.
  * The item is put as such, not copied. *)
  let put g content ofs len =
    g.length <- g.length + len ;
    Queue.add (content, ofs, len) g.buffers

  (*  Get [size] amount of data from [g].
  * Returns a list where each element will typically be passed to a blit:
  * its elements are of the form [b,o,o',l] where [o] is the offset of data
  * in the block [b], [o'] is the position at which it should be written
  * (the first position [o'] will always be [0]), and [l] is the length
  * of data to be taken from that block. *)
  let get g size =
    (* The main loop takes the current offset in the output buffer,
     * and iterates on input buffer chunks. *)
    let rec aux chunks offset =
      (* How much (more) data should be output? *)
      let needed = size - offset in
      assert (needed > 0) ;
      let block, block_ofs, block_len = Queue.peek g.buffers in
      let block_len = block_len - g.offset in
      let copied = min needed block_len in
      let chunks = (block, block_ofs + g.offset, offset, copied) :: chunks in
      (* Update buffer data -- did we consume a full block? *)
      if block_len <= needed then (
        ignore (Queue.take g.buffers) ;
        g.length <- g.length - block_len ;
        g.offset <- 0 )
      else (
        g.length <- g.length - needed ;
        g.offset <- g.offset + needed ) ;
      (* Add more data by recursing on the next block, or finish. *)
      if block_len < needed then aux chunks (offset + block_len)
      else List.rev chunks
    in
    if size = 0 then [] else aux [] 0
end

(* TODO: use this in the following modules instead of copying the code... *)
module Metadata = struct
  type t = {
    mutable metadata: (int * Frame.metadata) list;
    mutable breaks: int list;
    mutable length: int;
  }

  let create () = {metadata= []; breaks= []; length= 0}

  let clear g =
    g.metadata <- [] ;
    g.breaks <- [] ;
    g.length <- 0

  let advance g len =
    g.metadata <- List.map (fun (t, m) -> (t - len, m)) g.metadata ;
    g.metadata <- List.filter (fun (t, _) -> t >= 0) g.metadata ;
    g.breaks <- List.map (fun t -> t - len) g.breaks ;
    g.breaks <- List.filter (fun t -> t >= 0) g.breaks ;
    g.length <- g.length - len ;
    assert (g.length >= 0)

  let length g = g.length

  let remaining g = match g.breaks with a :: _ -> a | _ -> -1

  let metadata g len = List.filter (fun (t, _) -> t < len) g.metadata

  let feed_from_frame g frame =
    let size = Lazy.force Frame.size in
    let length = length g in
    g.metadata <-
      g.metadata
      @ List.map (fun (t, m) -> (length + t, m)) (Frame.get_all_metadata frame) ;
    g.breaks <-
      g.breaks
      @ List.map
          (fun t -> length + t)
          (* Filter out the last break, which only marks the end of frame, not a
           * track limit (doesn't mean is_partial). *)
          (List.filter (fun x -> x < size) (Frame.breaks frame)) ;
    let frame_length =
      let rec aux = function [t] -> t | _ :: tl -> aux tl | [] -> size in
      aux (Frame.breaks frame)
    in
    g.length <- g.length + frame_length

  let drop_initial_break g =
    match g.breaks with
      | 0 :: tl ->
          g.breaks <- tl
      | [] ->
          () (* end of stream / underrun... *)
      | _ ->
          assert false

  let fill g frame =
    let offset = Frame.position frame in
    let needed =
      let size = Lazy.force Frame.size in
      let remaining = remaining g in
      let remaining = if remaining = -1 then length g else remaining in
      min (size - offset) remaining
    in
    List.iter
      (fun (p, m) ->
        if p < needed then Frame.set_metadata frame (offset + p) m)
      g.metadata ;
    advance g needed ;
    (* Mark the end of this filling. If the frame is partial it must be because
     * of a break in the generator, or because the generator is emptying.
     * Conversely, each break in the generator must cause a partial frame, so
     * don't remove any if it isn't partial. *)
    Frame.add_break frame (offset + needed) ;
    if Frame.is_partial frame then drop_initial_break g
end

(** Generate a stream, including metadata and breaks.
  * The API is based on feeding from frames, and filling frames. *)
module From_frames = struct
  type t = {
    mutable metadata: (int * Frame.metadata) list;
    mutable breaks: int list;
    generator: Frame.content Generator.t;
  }

  let create () = {metadata= []; breaks= []; generator= Generator.create ()}

  let clear fg =
    fg.metadata <- [] ;
    fg.breaks <- [] ;
    Generator.clear fg.generator

  (** Total length. *)
  let length fg = Generator.length fg.generator

  (** Duration of data (in ticks) before the next break, -1 if there's none. *)
  let remaining fg = match fg.breaks with a :: _ -> a | _ -> -1

  let add_metadata fg m = fg.metadata <- fg.metadata @ [(length fg, m)]

  let add_break fg = fg.breaks <- fg.breaks @ [length fg]

  (* Advance metadata and breaks by [len] ticks. *)
  let advance fg len =
    let meta = List.map (fun (x, y) -> (x - len, y)) fg.metadata in
    let breaks = List.map (fun x -> x - len) fg.breaks in
    fg.metadata <- List.filter (fun x -> fst x >= 0) meta ;
    fg.breaks <- List.filter (fun x -> x >= 0) breaks

  let remove fg len =
    Generator.remove fg.generator len ;
    advance fg len

  (** Only the breaks and metadata in the considered portion of the
    * content will be taken into account. This includes position
    * ofs but excludes ofs+len for metadata and the opposite for breaks. *)
  let feed fg ?(copy = true) ?(breaks = []) ?(metadata = []) content ofs len =
    let breaks = List.filter (fun p -> ofs < p && p <= ofs + len) breaks in
    let metadata =
      List.filter (fun (p, _) -> ofs <= p && p < ofs + len) metadata
    in
    let content = if copy then Frame.copy content else content in
    fg.breaks <- fg.breaks @ List.map (fun p -> length fg + p - ofs) breaks ;
    fg.metadata <-
      fg.metadata @ List.map (fun (p, m) -> (length fg + p - ofs, m)) metadata ;
    Generator.put fg.generator content ofs len

  (** Take all data from a frame: breaks, metadata and available content. *)
  let feed_from_frame fg frame =
    let size = Lazy.force Frame.size in
    fg.metadata <-
      fg.metadata
      @ List.map
          (fun (p, m) -> (length fg + p, m))
          (Frame.get_all_metadata frame) ;
    fg.breaks <-
      fg.breaks
      @ List.map
          (fun p -> length fg + p)
          (* Filter out the last break, which only marks the end
           * of frame, not a track limit (doesn't mean is_partial). *)
          (List.filter (fun x -> x < size) (Frame.breaks frame)) ;
    (* Feed all content layers into the generator. *)
    let rec feed_all ofs = function
      | (cstop, content) :: contents ->
          Generator.put fg.generator (Frame.copy content) ofs cstop ;
          if cstop < size then feed_all cstop contents
      | [] ->
          assert false
    in
    feed_all 0 frame.Frame.contents

  (* Fill a frame from the generator's data. *)
  let fill fg frame =
    let offset = Frame.position frame in
    let buffer_size = Lazy.force Frame.size in
    let remaining =
      let l = remaining fg in
      if l = -1 then length fg else l
    in
    let needed = min (buffer_size - offset) remaining in
    let blocks = Generator.get fg.generator needed in
    List.iter
      (fun (block, o, o', size) ->
        let ctype = Frame.type_of_content block in
        let dst = Frame.content_of_type frame (offset + o') ctype in
        Frame.blit_content block o dst (offset + o') size)
      blocks ;
    List.iter
      (fun (p, m) ->
        if p < needed then Frame.set_metadata frame (offset + p) m)
      fg.metadata ;
    advance fg needed ;
    (* Mark the end of this filling.
     * If the frame is partial it must be because of a break in the
     * generator, or because the generator is emptying.
     * Conversely, each break in the generator must cause a partial frame,
     * so don't remove any if it isn't partial. *)
    Frame.add_break frame (offset + needed) ;
    if Frame.is_partial frame then (
      match fg.breaks with
        | 0 :: tl ->
            fg.breaks <- tl
        | [] ->
            () (* end of stream / underrun ... *)
        | _ ->
            assert false )
end

module From_audio_video = struct
  type mode = [`Audio | `Video | `Both | `Undefined]

  type t = {
    mutable mode: mode;
    audio: Frame.audio_t array Generator.t;
    video: Frame.video_t array Generator.t;
    mutable metadata: (int * Frame.metadata) list;
    mutable breaks: int list;
  }

  let create m =
    {
      mode= m;
      audio= Generator.create ();
      video= Generator.create ();
      metadata= [];
      breaks= [];
    }

  (** Audio length, in ticks. *)
  let audio_length t = Generator.length t.audio

  (** Video length, in ticks. *)
  let video_length t = Generator.length t.video

  (** Total length. *)
  let length t = min (Generator.length t.audio) (Generator.length t.video)

  let audio_size t =
    let float_size = 8 in
    let track_size (t : Frame.audio_t) = Audio.Mono.length t * float_size in
    let audio_size = Array.fold_left (fun n t -> n + track_size t) 0 in
    Queue.fold
      (fun n a -> n + audio_size (Generator.chunk_data a))
      0 t.audio.Generator.buffers

  let video_size t =
    let buffer_size (b : Frame.video_t array) =
      Array.fold_left (fun n v -> n + Video.size v) 0 b
    in
    Queue.fold
      (fun n a -> n + buffer_size (Generator.chunk_data a))
      0 t.video.Generator.buffers

  let size t = audio_size t + video_size t

  (** Duration of data (in ticks) before the next break, -1 if there's none. *)
  let remaining t = match t.breaks with a :: _ -> a | _ -> -1

  (** Add metadata at the minimum position of audio and video.
    * You probably want to call this when there is as much
    * audio as video. *)
  let add_metadata t m = t.metadata <- t.metadata @ [(length t, m)]

  (** Add a track limit. Audio and video length should be equal. *)
  let add_break ?(sync = `Strict) t =
    begin
      match sync with `Strict -> assert (audio_length t = video_length t)
      | `Ignore -> () | `Drop ->
          let alen = audio_length t in
          let vlen = video_length t in
          if alen > vlen then Generator.remove_end t.audio (alen - vlen) ;
          if vlen > alen then Generator.remove_end t.video (vlen - alen)
    end ;
    t.breaks <- t.breaks @ [length t]

  let clear t =
    t.metadata <- [] ;
    t.breaks <- [] ;
    Generator.clear t.audio ;
    Generator.clear t.video

  (** Current mode:
    * in Audio mode (resp. Video), only audio (resp. Audio) can be fed,
    * otherwise both have to be fed. *)
  let mode t = t.mode

  (** Change the generator mode. Only allowed when there is as much audio as video.  *)
  let set_mode t m =
    if t.mode <> m then (
      assert (audio_length t = video_length t) ;
      t.mode <- m )

  (** Add some audio content. Offset and length are given in audio samples. *)
  let put_audio t content o l =
    assert (content = [||] || Audio.Mono.length content.(0) >= o + l) ;
    let o = Frame.master_of_audio o in
    let l = Frame.master_of_audio l in
    Generator.put t.audio content o l ;
    match t.mode with
      | `Audio ->
          Generator.put t.video [||] 0 l
      | `Both ->
          ()
      | `Video | `Undefined ->
          assert false

  (** Add some video content. Offset and length are given in video samples. *)
  let put_video t content o l =
    assert (content = [||] || Video.length content.(0) <= o + l) ;
    let o = Frame.master_of_video o in
    let l = Frame.master_of_video l in
    Generator.put t.video content o l ;
    match t.mode with
      | `Video ->
          Generator.put t.audio [||] 0 l
      | `Both ->
          ()
      | `Audio | `Undefined ->
          assert false

  (** Take all data from a frame: breaks, metadata and available content. *)
  let feed_from_frame t frame =
    let size = Lazy.force Frame.size in
    t.metadata <-
      t.metadata
      @ List.map
          (fun (p, m) -> (length t + p, m))
          (Frame.get_all_metadata frame) ;
    t.breaks <-
      t.breaks
      @ List.map
          (fun p -> length t + p)
          (* Filter out the last break, which only marks the end
           * of frame, not a track limit (doesn't mean is_partial). *)
          (List.filter (fun x -> x < size) (Frame.breaks frame)) ;
    (* Feed all content layers into the generator. *)
    let rec feed_all ofs = function
      | (cstop, content) :: contents ->
          let content = Frame.copy content in
          ( match mode t with
            | `Audio ->
                put_audio t content.Frame.audio ofs cstop
            | `Video ->
                put_video t content.Frame.video ofs cstop
            | `Both ->
                put_audio t content.Frame.audio ofs cstop ;
                put_video t content.Frame.video ofs cstop
            | `Undefined ->
                () ) ;
          if cstop < size then feed_all cstop contents
      | [] ->
          assert false
    in
    feed_all 0 frame.Frame.contents

  (* Advance metadata and breaks by [len] ticks. *)
  let advance t len =
    let meta = List.map (fun (x, y) -> (x - len, y)) t.metadata in
    let breaks = List.map (fun x -> x - len) t.breaks in
    t.metadata <- List.filter (fun x -> fst x >= 0) meta ;
    t.breaks <- List.filter (fun x -> x >= 0) breaks

  let remove t len =
    Generator.remove t.audio len ;
    Generator.remove t.video len ;
    advance t len

  let fill t frame =
    let fpos = Frame.position frame in
    let size = Lazy.force Frame.size in
    let remaining =
      let l = remaining t in
      if l = -1 then length t else l
    in
    let l = min (size - fpos) remaining in
    let audio = Generator.get t.audio l in
    let video = Generator.get t.video l in
    (* We got equal durations of audio and video, but segmented differently.
     * We walk through them and blit them into the frame, possibly creating
     * several content layers of different types as the numbers of channels
     * vary. *)
    let rec blit audio video =
      match (audio, video) with
        | [], [] ->
            Frame.add_break frame (fpos + l)
        | (ablk, apos, apos', al) :: audio, (vblk, vpos, vpos', vl) :: video ->
            (* Audio and video destination positions are the same,
             * however they need be aligned.
             * At the beginning they are aligned (and zero), but then
             * we might advance from a few audio samples, which might
             * not correspond to an integer number of video samples,
             * after which vpos' is not the position of a video frame. *)
            assert (apos' = vpos') ;
            let fpos = fpos + apos' in
            let ctype =
              {
                Frame.audio= Array.length ablk;
                video= Array.length vblk;
                midi= 0;
              }
            in
            let dst = Frame.content_of_type frame fpos ctype in
            let l = min al vl in
            Utils.array_iter2 vblk dst.Frame.video (fun v v' ->
                let ( ! ) = Frame.video_of_master in
                (* How many samples should we output:
                 * the number of samples expected in total after this
                 * blit round, minus those that have been outputted before.
                 * When everything is aligned, this is the same as [!l]. *)
                let l = !(vpos' + l) - !vpos' in
                Video.blit v !vpos v' !fpos l) ;
            Utils.array_iter2 ablk dst.Frame.audio (fun a a' ->
                let ( ! ) = Frame.audio_of_master in
                (* Same as above, even if in practice everything
                 * will always be aligned on the audio side. *)
                let l = !(apos' + l) - !apos' in
                Audio.Mono.blit (Audio.Mono.sub a !apos l)
                  (Audio.Mono.sub a' !fpos l)) ;
            if al = vl then blit audio video
            else if al > vl then
              blit ((ablk, apos + vl, apos' + vl, al - vl) :: audio) video
            else blit audio ((vblk, vpos + al, vpos' + al, vl - al) :: video)
        | _, _ ->
            assert false
    in
    blit audio video ;
    List.iter
      (fun (p, m) -> if p < l then Frame.set_metadata frame (fpos + p) m)
      t.metadata ;
    advance t l ;
    (* If the frame is partial it must be because of a break in the
     * generator, or because the generator is emptying.
     * Conversely, each break in the generator must cause a partial frame,
     * so don't remove any if it isn't partial. *)
    if Frame.is_partial frame then (
      match t.breaks with
        | 0 :: tl ->
            t.breaks <- tl
        | [] ->
            () (* end of stream / underrun ... *)
        | _ ->
            assert false )
end

module From_audio_video_plus = struct
  module Super = From_audio_video

  type mode = [`Audio | `Video | `Both | `Undefined]

  (** There are different ways of handling an overful generator:
    * (1) when streaming, one should just stop the decoder for a while;
    * (2) when not streaming, one should throw some data.
    * Doing 1 instead of 2 can lead to deconnections.
    * Doing 2 instead of 1 leads to ugly sound.
    * Currently the only possibility is to drop data, since we want to remain
    * connected to the client. This behaves well in most cases, since clients
    * generally don't not go faster than stream-time. *)
  type overfull = [`Drop_old of int]

  type t = {
    lock: Mutex.t;
    (* The generator knows what content kind it is expected to produce
     * Because of the async put_audio/video calls, we only detect the
     * error upon [fill]. When an error is detected, the error flag is
     * set, which makes put_audio/video fail and hence kills the feeding
     * process. *)
    kind: Frame.content_kind;
    mutable error: bool;
    overfull: overfull option;
    gen: Super.t;
    log: string -> unit;
    (* Metadata rewriting, in place modification allowed *)
    mutable map_meta: Frame.metadata -> Frame.metadata;
  }

  let create ?(lock = Mutex.create ()) ?overfull ~kind ~log mode =
    {
      lock;
      kind;
      error= false;
      overfull;
      log;
      gen= Super.create mode;
      map_meta= (fun x -> x);
    }

  let mode t = Tutils.mutexify t.lock Super.mode t.gen

  let set_mode t mode = Tutils.mutexify t.lock (Super.set_mode t.gen) mode

  let audio_length t = Tutils.mutexify t.lock Super.audio_length t.gen

  let video_length t = Tutils.mutexify t.lock Super.video_length t.gen

  let length t = Tutils.mutexify t.lock Super.length t.gen

  let remaining t = Tutils.mutexify t.lock Super.remaining t.gen

  let audio_size t = Tutils.mutexify t.lock Super.audio_size t.gen

  let video_size t = Tutils.mutexify t.lock Super.video_size t.gen

  let size t = Tutils.mutexify t.lock Super.size t.gen

  let set_rewrite_metadata t f = t.map_meta <- f

  let add_metadata t m =
    Tutils.mutexify t.lock (fun m -> Super.add_metadata t.gen (t.map_meta m)) m

  let add_break ?sync t = Tutils.mutexify t.lock (Super.add_break ?sync) t.gen

  let clear t = Tutils.mutexify t.lock Super.clear t.gen

  let fill t frame =
    Tutils.mutexify t.lock
      (fun () ->
        let p = Frame.position frame in
        let breaks = Frame.breaks frame in
        Super.fill t.gen frame ;
        let _, c = Frame.content frame p in
        if not (Frame.type_has_kind (Frame.type_of_content c) t.kind) then (
          t.log "Incorrect stream type!" ;
          t.error <- true ;
          Super.clear t.gen ;
          Frame.clear_from frame p ;
          Frame.set_breaks frame (p :: breaks) ))
      ()

  let remove t len = Tutils.mutexify t.lock (Super.remove t.gen) len

  let check_overfull t extra =
    assert (Tutils.seems_locked t.lock) ;
    match t.overfull with
      | Some (`Drop_old len) when Super.length t.gen + extra > len ->
          let len = Super.length t.gen + extra - len in
          let len_time = Frame.seconds_of_master len in
          t.log
            (Printf.sprintf
               "Buffer overrun: Dropping %.2fs. Consider increasing the max \
                buffer size!"
               len_time) ;
          Super.remove t.gen len
      | _ ->
          ()

  let put_audio t buf off len =
    Tutils.mutexify t.lock
      (fun () ->
        if t.error then (
          Super.clear t.gen ;
          t.error <- false ;
          raise Incorrect_stream_type )
        else (
          check_overfull t (Frame.master_of_audio len) ;
          Super.put_audio t.gen buf off len ))
      ()

  let put_video t buf off len =
    Tutils.mutexify t.lock
      (fun () ->
        if t.error then (
          Super.clear t.gen ;
          t.error <- false ;
          raise Incorrect_stream_type )
        else (
          check_overfull t (Frame.master_of_video len) ;
          Super.put_video t.gen buf off len ))
      ()
end
