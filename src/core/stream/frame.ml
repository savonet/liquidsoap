(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

include Frame_settings
include Frame_base
open Content

(** Compatibilities between content kinds, types and values.
  * [sub a b] if [a] is more permissive than [b]..
  * TODO this is the other way around... it's correct in Lang, phew! *)

let map_fields fn = Fields.map fn
let mapi_fields fn = Fields.mapi fn
let string_of_format = string_of_format

let string_of_fields fn fields =
  Printf.sprintf "{%s}"
    (String.concat ","
       (Fields.fold
          (fun f v cur ->
            Printf.sprintf "%s=%s" (string_of_field f) (fn v) :: cur)
          fields []))

let string_of_content_type = string_of_fields string_of_format

module S = Set.Make (struct
  type t = Fields.key

  let compare = Stdlib.compare
end)

let compatible c c' =
  let f = List.map fst (Fields.bindings c) in
  let f' = List.map fst (Fields.bindings c') in
  S.(equal (of_list f) (of_list f'))
  && Fields.for_all (fun k v -> Content.compatible v (Fields.find k c')) c

(* Frames *)

let metadata_of_list l =
  let m = Hashtbl.create (List.length l) in
  List.iter (fun (k, v) -> Hashtbl.add m k v) l;
  m

type content = Content.data Fields.t

type t = {
  (* Presentation time, in multiple of frame size. *)
  mutable pts : int64 option;
  (* End of track markers.
   * A break at the end of the buffer is not an end of track.
   * So maybe we should rather call that an end-of-fill marker,
   * and notice that end-of-fills in the middle of a buffer are
   * end-of-tracks.
   * If needed, the end-of-track needs to be put at the beginning of
   * the next frame. *)
  mutable breaks : int list;
  (* Metadata can be put anywhere in the stream. *)
  mutable metadata : (int * metadata) list;
  mutable content : content;
}

(** Create a content chunk. All chunks have the same size. *)
let create_content ctype = map_fields (make ~length:!!size) ctype

let create ctype =
  { pts = None; breaks = []; metadata = []; content = create_content ctype }

let dummy = { pts = None; breaks = []; metadata = []; content = mk_fields () }
let content_type { content } = map_fields format content
let audio { content; _ } = Fields.find_opt audio_field content

let set_audio frame audio =
  frame.content <- Fields.add audio_field audio frame.content

let video { content; _ } = Fields.find_opt video_field content

let set_video frame video =
  frame.content <- Fields.add video_field video frame.content

let midi { content; _ } = Fields.find_opt midi_field content

let set_midi frame midi =
  frame.content <- Fields.add midi_field midi frame.content

(** Content independent *)

let position b = match b.breaks with [] -> 0 | a :: _ -> a
let is_partial b = position b < !!size
let breaks b = b.breaks
let set_breaks b breaks = b.breaks <- breaks
let add_break b br = b.breaks <- br :: b.breaks

let clear (b : t) =
  Fields.iter (fun _ c -> Content.clear c) b.content;
  b.breaks <- [];
  b.metadata <- []

let clear_from (b : t) pos =
  b.breaks <- List.filter (fun p -> p <= pos) b.breaks;
  b.metadata <- List.filter (fun (p, _) -> p <= pos) b.metadata

(** Presentation time stuff. *)

let pts { pts } = pts
let set_pts frame pts = frame.pts <- pts

(** Metadata stuff *)

exception No_metadata

let set_metadata b t m = b.metadata <- (t, m) :: b.metadata

let get_metadata b t =
  try Some (List.assoc t b.metadata) with Not_found -> None

let free_metadata b t =
  b.metadata <- List.filter (fun (tt, _) -> t <> tt) b.metadata

let free_all_metadata b = b.metadata <- []
let get_all_metadata b = List.sort (fun (x, _) (y, _) -> compare x y) b.metadata
let set_all_metadata b l = b.metadata <- l

let blit_content src src_pos dst dst_pos len =
  let blit src dst = blit src src_pos dst dst_pos len in
  Fields.iter (fun field src -> blit src (Fields.find field dst)) src

(** Copy data from [src] to [dst].
  * This triggers changes of contents layout if needed. *)
let blit src src_pos dst dst_pos len =
  (* Assuming that the tracks have the same track layout,
   * copy a chunk of data from [src] to [dst]. *)
  blit_content src.content src_pos dst.content dst_pos len

(** Raised by [get_chunk] when no chunk is available. *)
exception No_chunk

exception Not_equal

(** [get_chunk dst src] gets the (end of) next chunk from [src]
  * (a chunk is a region of a frame between two breaks).
  * Metadata relevant to the copied chunk is copied as well,
  * and content layout is changed if needed. *)
let get_chunk ab from =
  assert (is_partial ab);
  let p = position ab in
  let copy_chunk i =
    add_break ab i;
    blit from p ab p (i - p);

    (* If the last metadata before [p] differ in [from] and [ab],
     * copy the one from [from] to [p] in [ab].
     * NOTE (toots): This mechanism is super weird. See last test
       in src/test/frame_test.ml. I suspect that it is here b/c we
       have gotten into the habit of caching the last metadata at
       position -1 and that this is meant to surface it at position 0
       of the next chunk. I sure hope that we can revisit this mechanism
       at some point in the future.. *)
    begin
      let is_meta_equal m m' =
        try
          if Hashtbl.length m <> Hashtbl.length m' then raise Not_equal;
          Hashtbl.iter
            (fun v l ->
              match Hashtbl.find_opt m' v with
                | Some l' when l = l' -> ()
                | _ -> raise Not_equal)
            m;
          true
        with Not_equal -> false
      in
      let before_p l =
        match
          List.sort
            (fun (a, _) (b, _) -> compare b a) (* the greatest *)
            (List.filter (fun x -> fst x < p) l)
          (* that is less than p *)
        with
          | [] -> None
          | x :: _ -> Some (snd x)
      in
      match (before_p from.metadata, before_p ab.metadata) with
        | Some b, None -> set_metadata ab p b
        | Some b, Some a when not (is_meta_equal a b) -> set_metadata ab p b
        | _ -> ()
    end;

    (* Copy new metadata blocks for this chunk.
     * We exclude blocks at the end of chunk, leaving them to be copied
     * during the next get_chunk. *)
    List.iter
      (fun (mp, m) -> if p <= mp && mp < i then set_metadata ab mp m)
      from.metadata
  in
  let rec aux foffset f =
    (* We always have p >= foffset *)
    match f with
      | [] -> raise No_chunk
      | i :: tl ->
          (* Breaks are between ticks, they do range from 0 to size. *)
          assert (0 <= i && i <= !!size);
          if i = 0 && ab.breaks = [] then
            (* The only empty track that we copy,
             * trying to copy empty tracks in the middle could be useful
             * for packets like those forged by add, with a fake first break,
             * but isn't needed (yet) and is painful to implement. *)
            copy_chunk 0
          else if foffset <= p && i > p then copy_chunk i
          else aux i tl
  in
  aux 0 (List.rev from.breaks)
