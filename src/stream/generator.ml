(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** The base module doesn't even know what it is buffering. *)
module Generator =
struct

type 'a chunk = 'a * int * int
type 'a buffer = 'a chunk Queue.t

(** All positions and lengths are in ticks. *)
type 'a t = {
  mutable length  : int ;
  mutable offset  : int ;
  mutable buffers : 'a buffer
}

let create () =
  {
    length    = 0 ;
    offset    = 0 ;
    buffers   = Queue.create ()
  }

let clear g =
  g.length <- 0 ;
  g.offset <- 0 ;
  g.buffers <- Queue.create ()

let length b = b.length

(** Remove [len] ticks of data. *)
let rec remove g len =
  assert (g.length >= len) ;
  if len>0 then
  let b,_,b_len = Queue.peek g.buffers in
    (* Is it enough to advance in the first buffer?
     * Or do we need to consume it completely and go farther in the queue? *)
    if g.offset + len < b_len then begin
      g.length <- g.length - len ;
      g.offset <- g.offset + len ;
    end else
      let removed = b_len - g.offset in
        ignore (Queue.take g.buffers) ;
        g.length <- g.length - removed ;
        g.offset <- 0 ;
        remove g (len-removed)

(** Feed a content chunk into a generator. *)
let put g content ofs len =
  g.length <- g.length + len ;
  Queue.add (content,ofs,len) g.buffers

(** Get [size] amount of data from [g].
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
      assert (needed>0) ;
      let block,block_ofs,block_len = Queue.peek g.buffers in
      let block_len = block_len - g.offset in
      let copied = min needed block_len in
      let chunks = (block, block_ofs + g.offset, offset, copied)::chunks in
        (* Update buffer data -- did we consume a full block? *)
        if block_len <= needed then begin
          ignore (Queue.take g.buffers) ;
          g.length <- g.length - block_len ;
          g.offset <- 0
        end else begin
          g.length <- g.length - needed ;
          g.offset <- g.offset + needed
        end ;
        (* Add more data by recursing on the next block, or finish. *)
        if block_len < needed then
          aux chunks (offset+block_len)
        else
          List.rev chunks
  in
    if size = 0 then [] else aux [] 0

end

(** Generate a stream, including metadata and breaks.
  * The API is based on feeding from frames, and filling frames. *)
module From_frames =
struct

  type t = {
    mutable metadata : (int*Frame.metadata) list ;
    mutable breaks   : int list ;
    generator        : Frame.content Generator.t
  }

  let create () = {
    metadata = [] ;
    breaks = [] ;
    generator = Generator.create ()
  }

  let clear fg =
    fg.metadata <- [] ; fg.breaks <- [] ;
    Generator.clear fg.generator

  (** Total length. *)
  let length fg = Generator.length fg.generator

  (** Duration of data (in ticks) before the next break,
    * or total [length] otherwise. *)
  let remaining fg =
    match fg.breaks with
      | a :: _ -> a
      | _ -> length fg

  let add_metadata fg m =
    fg.metadata <- fg.metadata @ [length fg, m]

  let add_break fg =
    fg.breaks <- fg.breaks @ [length fg]

  (* Advance metadata and breaks by [len] ticks. *)
  let advance fg len =
    let meta = List.map (fun (x,y) -> (x-len,y)) fg.metadata in
    let breaks = List.map (fun x -> x-len) fg.breaks in
      fg.metadata <- List.filter (fun x -> fst x >= 0) meta;
      fg.breaks <- List.filter (fun x -> x >= 0) breaks

  let remove fg len =
    Generator.remove fg.generator len ;
    advance fg len

  let feed fg content ofs len =
    Generator.put fg.generator content ofs len

  (** Take all data from a frame: breaks, metadata and available content. *)
  let feed_from_frame fg frame =
    let size = Lazy.force Frame.size in
      fg.metadata <-
        fg.metadata @
          (List.map
             (fun (p,m) -> length fg + p, m)
             (Frame.get_all_metadata frame)) ;
      fg.breaks <-
        fg.breaks @
          (List.map
             (fun p -> length fg + p)
             (* Filter out the last break, which only marks the end
              * of frame, not a track limit (doesn't mean is_partial). *)
             (List.filter (fun x -> x < size) (Frame.breaks frame))) ;
      (* Feed all content layers into the generator. *)
      let rec feed_all ofs = function
        | (cstop,content)::contents ->
            Generator.put fg.generator content ofs cstop ;
            if cstop < size then
              feed_all cstop contents
        | [] -> assert false
      in
        feed_all 0 frame.Frame.contents

  (* Fill a frame from the generator's data. *)
  let fill fg frame =
    let offset = Frame.position frame in
    let buffer_size = Lazy.force Frame.size in
    let needed = min buffer_size (remaining fg) in
    let blocks = Generator.get fg.generator needed in
      List.iter
        (fun (block,o,o',size) ->
           let ctype = Frame.type_of_content block in
           let dst = Frame.content_of_type frame (offset+o') ctype in
             Frame.blit_content
               block o
               dst (offset+o')
               size)
        blocks ;
      List.iter
        (fun (p,m) ->
           if p < needed then
             Frame.set_metadata frame (offset+p) m)
        fg.metadata ;
      advance fg needed ;
      (* Mark the end of this filling.
       * If the frame is partial it must be because of a break in the
       * generator, or because the generator is emptying.
       * Conversely, each break in the generator must cause a partial frame,
       * so don't remove any if it isn't partial. *)
      Frame.add_break frame (offset+needed) ;
      if Frame.is_partial frame then
        match fg.breaks with
          | 0::tl -> fg.breaks <- tl
          | [] -> () (* end of stream / underrun ... *)
          | _ -> assert false

end

module From_audio_video =
struct

  type mode = Audio | Video | Both
  type t = {
    mutable mode : mode ;
    audio : Frame.audio_t array Generator.t ;
    video : Frame.video_t array Generator.t ;
    mutable metadata : (int*Frame.metadata) list ;
    mutable breaks   : int list
  }

  let create m = {
    mode = m ;
    audio = Generator.create () ;
    video = Generator.create () ;
    metadata = [] ;
    breaks = []
  }

  (** Audio length, in ticks. *)
  let audio_length t = Generator.length t.audio

  (** Video length, in ticks. *)
  let video_length t = Generator.length t.video

  (** Total length. *)
  let length t = min (Generator.length t.audio) (Generator.length t.video)

  (** Duration of data (in ticks) before the next break,
    * or total [length] otherwise. *)
  let remaining t =
    match t.breaks with
      | a :: _ -> a
      | _ -> length t

  (** Add metadata at the minimum position of audio and video.
    * You probably want to call this when there is as much
    * audio as video. *)
  let add_metadata t m =
    t.metadata <- t.metadata @ [length t, m]

  (** Add a track limit. Audio and video length should be equal. *)
  let add_break t =
    assert (audio_length t = video_length t) ;
    t.breaks <- t.breaks @ [length t]

  let clear t =
    t.metadata <- [] ; t.breaks <- [] ;
    Generator.clear t.audio ;
    Generator.clear t.video

  (** Current mode:
    * in Audio mode (resp. Video), only audio (resp. Audio) can be fed,
    * otherwise both have to be fed. *)
  let mode t = t.mode

  (** Change the generator mode.
    * Only allowed when there is as much audio as video.  *)
  let set_mode t m =
    assert (audio_length t = video_length t) ;
    t.mode <- m

  (** Add some audio content. Offset and length are given in audio samples. *)
  let put_audio t content o l =
    assert (content = [||] || Array.length content.(0) <= o+l) ;
    let o = Frame.master_of_audio o in
    let l = Frame.master_of_audio l in
      Generator.put t.audio content o l ;
      match t.mode with
        | Audio ->
            Generator.put t.video [||] 0 l
        | Both -> ()
        | Video -> assert false

  (** Add some video content. Offset and length are given in video samples. *)
  let put_video t content o l =
    assert (content = [||] || Array.length content.(0) <= o+l) ;
    let o = Frame.master_of_video o in
    let l = Frame.master_of_video l in
      Generator.put t.video content o l ;
      match t.mode with
        | Video ->
            Generator.put t.audio [||] 0 l
        | Both -> ()
        | Audio -> assert false

  (* Advance metadata and breaks by [len] ticks. *)
  let advance t len =
    let meta = List.map (fun (x,y) -> (x-len,y)) t.metadata in
    let breaks = List.map (fun x -> x-len) t.breaks in
      t.metadata <- List.filter (fun x -> fst x >= 0) meta;
      t.breaks <- List.filter (fun x -> x >= 0) breaks

  let remove t len =
    Generator.remove t.audio len ;
    Generator.remove t.video len ;
    advance t len

  let fill t frame =
    let fpos = Frame.position frame in
    let size = Lazy.force Frame.size in
    let l = min (size-fpos) (remaining t) in
    let audio = Generator.get t.audio l in
    let video = Generator.get t.video l in
    (* We got equal durations of audio and video, but segmented differently.
     * We walk through them and blit them into the frame, possibly creating
     * several content layers of different types as the numbers of channels
     * vary. *)
    let rec blit audio video =
      match audio, video with
        | [],[] ->
            Frame.add_break frame (fpos+l)
        | (ablk,apos,apos',al)::audio,
          (vblk,vpos,vpos',vl)::video ->
            (* Audio and video destination positions are the same,
             * however they need be aligned.
             * At the beginning they are aligned (and zero), but then
             * we might advance from a few audio samples, which might
             * not correspond to an integer number of video samples,
             * after which vpos' is not the position of a video frame. *)
            assert (apos'=vpos') ;
            let fpos = fpos+apos' in
            let ctype =
              { Frame.
                audio = Array.length ablk ;
                video = Array.length vblk ;
                midi  = 0 }
            in
            let dst = Frame.content_of_type frame fpos ctype in
            let l = min al vl in
              Utils.array_iter2 vblk dst.Frame.video
                (fun v v' ->
                   let (!) = Frame.video_of_master in
                   (* How many samples should we output:
                    * the number of samples expected in total after this
                    * blit round, minus those that have been outputted before.
                    * When everything is aligned, this is the same as [!l]. *)
                   let l = !(vpos'+l) - !vpos' in
                     for i = 0 to l-1 do
                       RGB.blit_fast v.(!vpos+i) v'.(!fpos+i)
                     done) ;
              Utils.array_iter2 ablk dst.Frame.audio
                (fun a a' ->
                   let (!) = Frame.audio_of_master in
                   (* Same as above, even if in practice everything
                    * will always be aligned on the audio side. *)
                   let l = !(apos'+l) - !apos' in
                     Float_pcm.blit a !apos a' !fpos l) ;
              if al=vl then
                blit audio video
              else if al>vl then
                blit ((ablk,apos+vl,apos'+vl,al-vl)::audio) video
              else
                blit audio ((vblk,vpos+al,vpos'+al,vl-al)::video)
        | _,_ -> assert false
    in
      blit audio video ;
      List.iter
        (fun (p,m) ->
           if p<l then
             Frame.set_metadata frame (fpos+p) m)
        t.metadata ;
      advance t l ;
      (* If the frame is partial it must be because of a break in the
       * generator, or because the generator is emptying.
       * Conversely, each break in the generator must cause a partial frame,
       * so don't remove any if it isn't partial. *)
      if Frame.is_partial frame then
        match t.breaks with
          | 0::tl -> t.breaks <- tl
          | [] -> () (* end of stream / underrun ... *)
          | _ -> assert false

end
