(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

type float_pcm = float array

type float_pcm_t = float (* samplerate *)

type track_t =
  | Float_pcm_t of float_pcm_t
  | Midi_t of Midi.header
  | RGB_t

type track =
  | Float_pcm of (float_pcm_t * float_pcm)
  | Midi of (Midi.header * Midi.track ref)
  | RGB of RGB.t array

type metadata = (string,string) Hashtbl.t

type t =
    {
      freq : int; (* ticks per second *)
      length : int; (* length in ticks *)
      mutable tracks : track array;
      (* End of track markers.
       * A break at the end of the buffer is not an end of track.
       * So maybe we should rather call that an end-of-fill marker,
       * and notice that end-of-fills in the middle of a buffer are
       * end-of-tracks.
       * If needed, the end-of-track needs to be put at the beginning of
       * the next frame. *)
      mutable breaks : int list;
      (* Metadata can be put anywhere in the stream. *)
      mutable metadata : (int * metadata) list
    }

let create_track freq length = function
  | Float_pcm_t f ->
      let len =
        if int_of_float f = freq then
          length
        else
          int_of_float (((float length) /. (float freq)) *. f)
      in
        Float_pcm (f, Array.create len 0.)
  | Midi_t h ->
      Midi (h, ref (Midi.create_track ()))
  | RGB_t ->
      RGB (Array.init
             (Fmt.video_frames_per_frame ())
             (fun _ -> RGB.create (Fmt.video_width ()) (Fmt.video_height ())))

let create kind ~freq ~length =
  {
    freq = freq;
    length = length;
    tracks = Array.map (create_track freq length) kind;
    breaks = [];
    metadata = [];
  }

(* Create a standard frame. *)
let make () =
  let audio =
    Array.init (Fmt.channels ())
      (fun _ ->
         Float_pcm_t (float (Fmt.samples_per_second()))
      )
  in
  let video =
    Array.init (Fmt.video_channels ()) (fun _ -> RGB_t)
  in
    create
      (Array.append audio video)
      (Fmt.ticks_per_second ())
      (Fmt.ticks_per_frame ())


let kind_of_track = function
  | Float_pcm (t, _) -> Float_pcm_t t
  | Midi (h, _) -> Midi_t h
  | RGB _ -> RGB_t

let kind b = Array.map kind_of_track b.tracks

let get_tracks b = b.tracks

let add_track b t =
  b.tracks <- Array.append [|t|] b.tracks

let duration b = (float b.length) /. (float b.freq)

let size b = b.length

let position b =
  match b.breaks with
    | [] -> 0
    | a::_ -> a

let is_partial b = position b < size b
let clear b = b.breaks <- []; b.metadata <- []

(* Same as clear but leaves the last metadata at position -1. *)
let advance b =
  b.breaks <- [] ;
  let max a (p,m) =
    match a with Some (pa,ma) when pa > p -> a | _ -> Some (p,m)
  in
  let rec last a = function
    | [] -> a
    | b::l -> last (max a b) l
  in
    b.metadata <-
      match last None b.metadata with
        | None -> []
        | Some (_,e) -> [-1,e]

let breaks b = b.breaks
let set_breaks b breaks = b.breaks <- breaks
let add_break b br = b.breaks <- br::b.breaks

(** Metadata stuff *)

exception No_metadata
let set_metadata b t m = b.metadata <- (t,m)::b.metadata
let get_metadata b t =
  try
    Some (List.assoc t b.metadata)
  with Not_found -> None
let free_metadata b t =
  b.metadata <- List.filter (fun (tt,_) -> t<>tt) b.metadata
let free_all_metadata b = b.metadata <- []
let get_all_metadata b = List.filter (fun (x,_) -> x <> -1) b.metadata
let set_all_metadata b l = b.metadata <- l


(** Chunks *)
exception No_chunk

external float_blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let blit src src_pos dst dst_pos len =
  (* Assuming that the tracks have the same track layout,
   * copy a chunk of data from [src] to [dst]. *)
  for j = 0 to Array.length src.tracks - 1 do
    match src.tracks.(j), dst.tracks.(j) with
      | Float_pcm (f, a), Float_pcm (f', a') ->
          assert (f = f' && src.freq = dst.freq) ;
          let r = f /. float src.freq in
          let c x = int_of_float (float x *. r) in
            float_blit a (c src_pos) a' (c dst_pos) (c len)
      | Midi (_,m), Midi (h,m') -> m' := !m
      | RGB src, RGB dst ->
          (* TODO: handle offsets! *)
          for i = 0 to Array.length src - 1 do
            RGB.blit_fast src.(i) dst.(i)
          done
      | _, _ -> assert false
  done

let log = Dtools.Log.make ["frame"]

(* Get the (end of) next chunk from [from].
 * A chunk is a region of a frame between two breaks.
 * Metadata relevant to the copied chunk is copied as well. *)
let get_chunk ab from =
  assert (is_partial ab);
  let p = position ab in
  let copy_chunk i =
    add_break ab i ;
    blit from p ab p (i-p) ;
    (* If the last metadata before [p] differ in [from] and [ab],
     * copy the one from [from] to [p] in [ab].
     * Note: equality probably does not make much sense for hash tables,
     * but even physical equality should work here, it seems.. *)
    begin
      let before_p l =
        match
          List.sort
            (fun (a,_) (b,_) -> compare b a)     (* the greatest *)
            (List.filter (fun x -> fst x < p) l) (* that is less than p *)
        with [] -> None | x::_ -> Some (snd x)
      in
        match before_p from.metadata, before_p ab.metadata with
          | Some b, a ->
              if a <> Some b then set_metadata ab p b
          | None, _ -> ()
    end ;
    (* Copy new metadata blocks for this chunk.
     * We exclude blocks at the end of chunk, leaving them to be copied
     * during the next get_chunk. *)
    List.iter
      (fun (mp,m) ->
         if p<=mp && mp<i then
           set_metadata ab mp m)
      from.metadata ;
  in
  let rec aux foffset f =
    (* We always have p >= foffset *)
    match f with
      | [] -> raise No_chunk
      | i::tl ->
          (* Breaks are between ticks, they do range from 0 to size. *)
          assert (0<=i && i<=(size ab));
          if i = 0 && ab.breaks = [] then
            (* The only empty track that we copy,
             * trying to copy empty tracks in the middle could be useful
             * for packets like those forged by add, with a fake first break,
             * but isn't needed (yet) and is painful to implement. *)
            copy_chunk 0
          else if foffset < i && i > p then begin
            copy_chunk i
          end else
            aux i tl
  in
    aux 0 (List.rev from.breaks)

let fill_from_marshal stream frame =
    assert(is_partial frame);
    let cur_pos = position frame in
    (* Yes, this might lose some data.
     * However, it is very simple this way,
     * and avoid either a local buffer or a
     * send->receive paradigm with the other end.. *)
    let rec get () =
      let (nframe : t) = Marshal.from_channel stream in
      if position nframe < cur_pos then
        get ()
      else
        nframe
    in
    let nframe = get () in
    let new_pos = position nframe in
    let len = new_pos - cur_pos in
    blit nframe cur_pos frame cur_pos len;
    let new_meta = get_all_metadata nframe in
    let cur_meta = get_all_metadata frame in
    let add_meta (p,m) =
      match p with
        (* Last kept metadata should always be the more recent as possible.. *)
        | -1 ->
             if not (List.mem_assoc (-1)
                      (get_all_metadata frame))
             then
               set_metadata frame (-1) m
        | p when p >= cur_pos ->
             set_metadata frame p m
        (** The perfectionist's addition:
          * Add a metadata in the worst case.. *)
        | p when
              (* No old metadata *)
              cur_meta = [] &&
              (* No new metadata, or kept metadata *)
              not (List.exists
                     (fun (x,_) -> (x >= cur_pos) || (x = -1))
                     new_meta) &&
              (* No metadata was already added at cur_pos.. *)
              not (List.mem_assoc cur_pos
                    (get_all_metadata frame))
                        ->
             (* Add this metadata at cur_pos. Since
              * the list starts with the oldest one,
              * this should always add the latest one,
              * though I doubt another situation will
              * ever happen.. *)
             set_metadata frame cur_pos m
        | _ -> ()
    in
    List.iter add_meta new_meta;
    add_break frame new_pos
