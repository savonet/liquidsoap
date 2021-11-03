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

(** The buffer is always kept in sync as follows:
    - PTS in audio and video is expected to be tracked when submitting
      data. User with no knowledge of PTS (typically all decoders except
      ffmpeg) should be able to submit without having to deal with PTS.
    - The buffer is always in sync except for potentially one type of
      data and always at the end of the buffer. Typically:
         0----1----2--> audio
         0----1----2----3----4----> video
    - When a new chunk is added, any gap in data from the other type
      is removed. So, for instance, when adding an audio chunk of type
      4--(5)----(6)- one gets:
         0----1----4----5----6-> audio
         0----1----4----> video
      Note: video frame being usually one frame long, we unfortunately cannot
      keep partially filled frames.
    - Current chunk length and PTS are tracked so that new chunk are split
      by increment of frame size. Typically, if we have:
         0----1----2--> audio
         0----1----2----3----4----5> video
      Adding a chunk of audio of type: 2--(3)----(4)- should
      result in  adding a first chunk of 4-- then a chunk of 3----
      and, finally, a chunk of 4-. *)

type mode = [ `Audio | `Video | `Both | `Undefined ]

type pending = {
  frames : (int64 * Content.data) Queue.t;
  mutable pts : int64;
  mutable pending : Content.data option;
}

type t = {
  m : Mutex.t;
  mutable mode : mode;
  mutable synced : Content.data option;
  audio : pending;
  video : pending;
  mutable metadata : Content.data;
  mutable breaks : Content.data;
}

let create ?overfull ?log ?log_overfull mode =
  {
    m = Mutex.create ();
    mode;
    synced = None;
    audio = { frames = Queue.create (); pts = 0L; pending = None };
    video = { frames = Queue.create (); pts = 0L; pending = None };
    metadata = Content.make ~size:0 Content.Metadata.format;
    breaks = Content.make ~size:0 Content.Breaks.format;
  }

let mode g = Tutils.mutexify g.m (fun () -> g.mode) ()
let set_mode g m = Tutils.mutexify g.m (fun () -> g.mode <- m) ()

let clear_pending p =
  Queue.clear p.frames;
  p.pending <- None

let clear_async g =
  clear_pending g.audio;
  clear_pending g.video;
  g.metadata <- Content.sub g.metadata 0 0;
  g.breaks <- Content.sub g.breaks 0 0

let clear g =
  Tutils.mutexify g.m
    (fun () ->
      clear_async g;
      g.synced <- None)
    ()

let content_length = function None -> 0 | Some c -> Content.length c

let pending_length { frames; pending } =
  Queue.fold
    (fun cur (_, d) -> cur + Content.length d)
    (match pending with None -> 0 | Some d -> Content.length d)
    frames

let audio_length g =
  Tutils.mutexify g.m
    (fun () -> content_length g.synced + pending_length g.audio)
    ()

let video_length g =
  Tutils.mutexify g.m
    (fun () -> content_length g.synced + pending_length g.video)
    ()

let _length g = content_length g.synced
let length g = Tutils.mutexify g.m (fun () -> _length g) ()

let buffered_length g =
  Tutils.mutexify g.m
    (fun () ->
      content_length g.synced
      + max (pending_length g.audio) (pending_length g.video))
    ()

let _remaining g =
  match g.synced with
    | None -> 0
    | Some synced -> (
        match List.rev (Content.Frame.get_breaks synced) with
          | a :: _ -> a
          | _ -> -1)

let remaining g = Tutils.mutexify g.m (fun () -> _remaining g) ()

let add_metadata ?(pos = 0) g m =
  Tutils.mutexify g.m
    (fun () ->
      let size = max pos (Content.length g.metadata) in
      g.metadata <-
        Content.Metadata.(lift_data ~size ((pos, m) :: get_data g.metadata)))
    ()

let add_break ?(sync = false) ?(pos = 0) g =
  Tutils.mutexify g.m
    (fun () ->
      if sync then clear_async g;
      let size = max pos (Content.length g.breaks) in
      g.breaks <- Content.Breaks.(lift_data ~size (pos :: get_data g.breaks)))
    ()

let rec sync_content g =
  let size = Lazy.force Frame.size in
  match (Queue.peek_opt g.audio.frames, Queue.peek_opt g.video.frames) with
    | Some (p, audio), Some (p', video) when p = p' ->
        let metadata = Content.make ~size Content.Metadata.format in
        let len = min size (Content.length g.metadata) in
        Content.blit g.metadata 0 metadata 0 len;
        g.metadata <-
          Content.sub g.metadata len (Content.length g.metadata - len);

        let breaks = Content.make ~size Content.Breaks.format in
        let len = min size (Content.length g.breaks) in
        Content.blit g.breaks 0 breaks 0 len;
        g.breaks <- Content.sub g.breaks len (Content.length g.breaks - len);

        let c =
          Content.Frame.(
            lift_data
              {
                breaks;
                metadata;
                media =
                  {
                    Frame.audio;
                    video;
                    midi = Content.make ~size Content.None.format;
                  };
              })
        in
        g.synced <-
          (match g.synced with
            | None -> Some c
            | Some synced -> Some (Content.append synced c));
        ignore (Queue.take g.audio.frames);
        ignore (Queue.take g.video.frames);
        sync_content g
    | Some (p, _), Some (p', _) when p < p' ->
        ignore (Queue.take g.audio.frames);
        sync_content g
    | Some (p, _), Some (p', _) when p' < p ->
        ignore (Queue.take g.video.frames);
        sync_content g
    | _ -> ()

let put_data ?pts p data =
  let pts = Option.value ~default:p.pts pts in
  if p.pts <= pts then (
    if p.pts < pts then p.pending <- None;
    p.pts <- pts;
    let data =
      match p.pending with None -> data | Some d -> Content.append d data
    in
    let size = Lazy.force Frame.size in
    let rec f data =
      let len = Content.length data in
      if size <= len then (
        Queue.push (p.pts, Content.sub data 0 size) p.frames;
        p.pts <- Int64.succ p.pts;
        f (Content.sub data size (len - size)))
      else p.pending <- Some data
    in
    f data)

let put_audio ?pts g data ofs len =
  Tutils.mutexify g.m
    (fun () ->
      put_data ?pts g.audio (Content.sub data ofs len);

      (match g.mode with
        | `Audio -> put_data ?pts g.video (Content.None.data len)
        | `Both -> ()
        | _ -> assert false);

      sync_content g)
    ()

let put_video ?pts g data ofs len =
  Tutils.mutexify g.m
    (fun () ->
      put_data ?pts g.video (Content.sub data ofs len);

      (match g.mode with
        | `Video -> put_data ?pts g.audio (Content.None.data len)
        | `Both -> ()
        | _ -> assert false);

      sync_content g)
    ()

let feed ?(copy : [ `None | `Audio | `Video | `Both ] = `Both) ?mode g content
    ofs len =
  let mode = Option.value ~default:g.mode mode in

  if mode = `Audio || mode = `Both then (
    let audio = Content.Frame.get_audio content in
    let audio =
      if copy = `Audio || copy = `Both then Content.copy audio else audio
    in
    put_audio g audio ofs len);

  if mode = `Video || mode = `Both then (
    let video = Content.Frame.get_video content in
    let video =
      if copy = `Video || copy = `Both then Content.copy video else video
    in
    put_video g video ofs len)

let feed_from_frame ?copy ?mode g frame =
  feed ?copy ?mode g (Frame.content frame) 0 (Frame.position frame)

let _remove g len =
  g.synced <-
    Some (Content.sub (Option.get g.synced) len (content_length g.synced - len))

let remove g len = Tutils.mutexify g.m (fun () -> _remove g len) ()

let get g len =
  Tutils.mutexify g.m
    (fun () ->
      assert (len <= length g);
      let c = Content.sub (Option.get g.synced) 0 len in
      remove g len;
      c)
    ()

let fill g frame =
  Tutils.mutexify g.m
    (fun () ->
      let len =
        match _remaining g with -1 -> content_length g.synced | len -> len
      in
      let pos = Frame.position frame in
      let size = Lazy.force Frame.size in
      let len = min (size - pos) len in
      let b = Frame.breaks frame in
      let m = Frame.get_all_metadata frame in
      if 0 < len then (
        Content.blit (Option.get g.synced) 0 (Frame.content frame) pos len;
        _remove g len);
      (* TODO: Frame content erases all breaks and metadata. We add this to be backward compatible
         with existing call sites. This will be better once we cleanup the streaming API. *)
      Frame.set_breaks frame ((pos + len) :: b);
      Frame.set_all_metadata frame (m @ Frame.get_all_metadata frame))
    ()
