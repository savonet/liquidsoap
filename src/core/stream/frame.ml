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

include Frame_settings
include Frame_base
open Content

(** Compatibilities between content kinds, types and values.
  * [sub a b] if [a] is more permissive than [b]..
  * TODO this is the other way around... it's correct in Lang, phew! *)

let string_of_format = string_of_format

let string_of_fields fn fields =
  Printf.sprintf "{%s}"
    (String.concat ","
       (List.sort Stdlib.compare
          (Fields.fold
             (fun f v cur ->
               Printf.sprintf "%s=%s" (Fields.string_of_field f) (fn v) :: cur)
             fields [])))

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

let list_of_metadata h = Hashtbl.fold (fun k v l -> (k, v) :: l) h []

type t = Generator.t

let create content_type = Generator.create ~length:!!size content_type
let dummy () = create Fields.empty
let content_type = Generator.content_type
let get frame field = Generator.get_field frame field
let set = Generator.set_field
let audio frame = get frame Fields.audio
let set_audio frame content = set frame Fields.audio content
let video frame = get frame Fields.video
let set_video frame content = set frame Fields.video content
let midi frame = get frame Fields.midi
let set_midi frame content = set frame Fields.midi content

(** Content independent *)

let breaks frame = Content.Track_marks.get_data (get frame Fields.track_marks)

let set_breaks frame =
  Content.Track_marks.set_data (get frame Fields.track_marks)

let add_break b br = set_breaks b (br :: breaks b)
let position frame = match List.rev (breaks frame) with [] -> 0 | a :: _ -> a
let remaining b = !!size - position b
let is_partial b = 0 < remaining b
let clear b = Fields.iter (fun _ c -> Content.clear c) (Generator.peek b)

(** Metadata stuff *)

exception No_metadata

let get_all_metadata frame =
  Content.Metadata.get_data (get frame Fields.metadata)

let set_all_metadata frame data =
  let data = List.sort_uniq (fun (p, _) (p', _) -> Stdlib.compare p p') data in
  Content.Metadata.set_data (get frame Fields.metadata) data

let set_metadata b t m =
  set_all_metadata b
    ((t, m) :: List.filter (fun (p, _) -> p <> t) (get_all_metadata b))

let get_metadata b t =
  try Some (List.assoc t (get_all_metadata b)) with Not_found -> None

let free_metadata b t =
  set_all_metadata b (List.filter (fun (tt, _) -> t <> tt) (get_all_metadata b))

let free_all_metadata b = set_all_metadata b []

let blit_content src src_pos dst dst_pos len =
  let blit src dst = blit src src_pos dst dst_pos len in
  Fields.iter
    (fun field dst ->
      if field <> Fields.track_marks && field <> Fields.metadata then
        blit (Fields.find field src) dst)
    dst

let blit src src_pos dst dst_pos len =
  blit_content (Generator.peek src) src_pos (Generator.peek dst) dst_pos len

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
      match
        (before_p (get_all_metadata from), before_p (get_all_metadata ab))
      with
        | Some b, None -> set_metadata ab p b
        | Some b, Some a when not (is_meta_equal a b) -> set_metadata ab p b
        | _ -> ()
    end;

    (* Copy new metadata blocks for this chunk.
     * We exclude blocks at the end of chunk, leaving them to be copied
     * during the next get_chunk. *)
    List.iter
      (fun (mp, m) -> if p <= mp && mp < i then set_metadata ab mp m)
      (get_all_metadata from)
  in
  let rec aux foffset f =
    (* We always have p >= foffset *)
    match f with
      | [] -> raise No_chunk
      | i :: tl ->
          (* Breaks are between ticks, they do range from 0 to size. *)
          assert (0 <= i && i <= !!size);
          if i = 0 && breaks ab = [] then
            (* The only empty track that we copy,
             * trying to copy empty tracks in the middle could be useful
             * for packets like those forged by add, with a fake first break,
             * but isn't needed (yet) and is painful to implement. *)
            copy_chunk 0
          else if foffset <= p && i > p then copy_chunk i
          else aux i tl
  in
  aux 0 (breaks from)
