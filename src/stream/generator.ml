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

(** A generator is used to accumulate and replay a stream.
  * It consumes and produces frames. *)

type chunk = Frame.content * int * int
type buffer = chunk Queue.t

(** All positions and lengths are in ticks. *)
type t = {
  mutable metadata : (int*Frame.metadata) list ;
  mutable breaks   : int list ;
  mutable length   : int ;
  mutable offset   : int ;
  mutable buffers  : buffer ;
}

let create () =
  {
    metadata  = [] ;
    breaks    = [] ;
    length    = 0 ;
    offset    = 0 ;
    buffers   = Queue.create ()
  }

let clear abg =
  abg.metadata <- [] ;
  abg.breaks <- [] ;
  abg.length <- 0 ;
  abg.offset <- 0 ;
  abg.buffers <- Queue.create ()

let length b = b.length

let remaining abg =
  match abg.breaks with
    | a :: _ -> a
    | _ -> abg.length

let add_metadata abg m =
  abg.metadata <- abg.metadata @ [abg.length, m]

let add_break abg =
  abg.breaks <- abg.breaks @ [abg.length]

(* Advance metadata and breaks by [pos] ticks. *)
let advance abg pos =
  let meta = List.map (fun (x,y) -> (x-pos,y)) abg.metadata in
  let breaks = List.map (fun x -> x-pos) abg.breaks in
    abg.metadata <- List.filter (fun x -> fst x >= 0) meta;
    abg.breaks <- List.filter (fun x -> x >= 0) breaks

(** Remove [len] ticks of data. *)
let rec remove abg len =
  assert (abg.length >= len) ;
  if len>0 then
  let b,_,b_len = Queue.peek abg.buffers in
    (* Is it enough to advance in the first buffer?
     * Or do we need to consume it completely and go farther in the queue? *)
    if abg.offset + len < b_len then begin
      abg.length <- abg.length - len ;
      abg.offset <- abg.offset + len ;
      advance abg len
    end else
      let removed = b_len - abg.offset in
        ignore (Queue.take abg.buffers) ;
        abg.length <- abg.length - removed ;
        abg.offset <- 0 ;
        advance abg removed ;
        remove abg (len-removed)

(** Feed a content chunk into a generator. This won't be used directly. *)
let feed abg content ofs len =
  (* assert (Frame.type_has_kind (Frame.type_of_content content) abg.kind) ; *)
  abg.length <- abg.length + len ;
  Queue.add (content,ofs,len) abg.buffers

(** Take all data from a frame: breaks, metadata and available content. *)
let feed_from_frame abg frame =
  (* assert (Frame.kind_sub_kind frame.kind abg.kind) ; *)
  let size = Lazy.force Frame.size in
    abg.metadata <-
      abg.metadata @
        (List.map
           (fun (p,m) -> abg.length + p, m)
           (Frame.get_all_metadata frame)) ;
    abg.breaks <-
      abg.breaks @
        (List.map
           (fun p -> abg.length + p)
           (* TODO Why this filter? *)
           (List.filter (fun x -> x < size) (Frame.breaks frame))) ;
    (* Feed all content layers into the generator. *)
    let rec feed_all ofs len = function
      | (clen,content)::contents ->
          feed abg content ofs (min clen len) ;
          if clen<len then
            feed_all (ofs+clen) (len-clen) contents
      | [] -> assert false
    in
      feed_all 0 size frame.Frame.contents

let feed_from_pcm ~sample_freq abg buf =
  (* assert (Frame.type_has_kind
            { Frame.audio = Array.length buf ;
              Frame.video = 0 ; Frame.midi = 0 }
            abg.kind) ; *)
  let conv = Audio_converter.Samplerate.create (Array.length buf) in
  let resample  = Audio_converter.Samplerate.resample conv in
  let buf =
    resample
      (float (Lazy.force Frame.audio_rate) /. float sample_freq)
      buf 0 (Array.length buf.(0))
  in
  let content =
    {
      Frame.audio = buf ;
      Frame.video = [||] ; Frame.midi = [||]
    }
  in
    feed abg content 0 (Frame.master_of_audio (Array.length buf.(0)))

(* Fill a frame from the generator's data. *)
let fill abg frame =
  let offset = Frame.position frame in
  let buffer_size = Lazy.force Frame.size in
  (* The main loop takes the current offset in the output buffer,
   * and iterates on input buffer chunks. *)
  let rec aux offset =
    (* How much (more) data should be output? *)
    let needed = min (remaining abg) (buffer_size - offset) in
      if needed = 0 then begin
        Frame.add_break frame offset ;
        if Frame.is_partial frame then
          match abg.breaks with
            | 0::tl -> abg.breaks <- tl
            | [] -> () (* end of stream / underrun ... *)
            | _ -> assert false
      end else
        let block,block_ofs,block_len = Queue.peek abg.buffers in
        let block_len = block_len - abg.offset in
        let copied = min needed block_len in
        let dst =
          Frame.content_of_type frame offset (Frame.type_of_content block)
        in
          Frame.blit_content
            block (block_ofs+abg.offset)
            dst offset
            copied ;
          List.iter
            (fun (p,m) ->
               if p < copied then
                 Frame.set_metadata frame (p + offset) m)
            abg.metadata ;
          advance abg copied ;
          (* Update buffer data -- did we consume a full block? *)
          if block_len <= needed then begin
            ignore (Queue.take abg.buffers) ;
            abg.length <- abg.length - block_len ;
            abg.offset <- 0
          end else begin
            abg.length <- abg.length - needed ;
            abg.offset <- abg.offset + needed
          end ;
          (* Add more data by recursing on the next block, or finish. *)
          if block_len < needed then
            aux (offset+block_len)
          else begin
            Frame.add_break frame (offset+needed) ;
            if Frame.is_partial frame then
              match abg.breaks with
                | 0::tl -> abg.breaks <- tl
                | [] -> () (* end of stream / underrun ... *)
                | _ -> assert false
          end
  in
    aux offset
