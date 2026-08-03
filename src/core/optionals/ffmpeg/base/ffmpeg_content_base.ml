(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let conf_ffmpeg_content =
  Dtools.Conf.void
    ~p:(Ffmpeg_utils.conf_ffmpeg#plug "content")
    "FFmpeg content configuration"

let stream_idx = ref 0L

let new_stream_idx () =
  stream_idx := Int64.succ !stream_idx;
  !stream_idx

type 'b data = {
  length : int;
  stream_idx : Int64.t;
  time_base : Avutil.rational;
  data : (int * 'b) list;
}

type ('a, 'b) content = { mutable params : 'a; mutable chunks : 'b data list }

let sort_data data = List.sort (fun (p1, _) (p2, _) -> compare p1 p2) data

let make_data ?(length = 0) ?(stream_idx = 0L)
    ?(time_base = { Avutil.num = 1; den = 1 }) ?(data = []) () =
  { length; stream_idx; time_base; data = sort_data data }

let make params : ('a, 'b) content = { params; chunks = [] }
let params { params; _ } = params

let length { chunks; _ } =
  List.fold_left (fun cur chunk -> cur + chunk.length) 0 chunks

let copy_data ~copy { length; stream_idx; time_base; data } =
  {
    length;
    stream_idx;
    time_base;
    data = sort_data (List.map (fun (pos, p) -> (pos, copy p)) data);
  }

let copy ~copy ({ params; chunks } : ('a, 'b) content) : ('a, 'b) content =
  { params; chunks = List.map (copy_data ~copy) chunks }

let sub_data ~copy { stream_idx; time_base; data; _ } offset len =
  let stop = offset + len in
  let filtered =
    List.filter_map
      (fun (pos, p) ->
        if offset <= pos && pos < stop then Some (pos - offset, copy p)
        else None)
      data
  in
  { length = len; stream_idx; time_base; data = sort_data filtered }

let collapse_chunks chunks =
  let rec aux acc = function
    | [] -> List.rev acc
    | (dst_offset, chunk) :: rest -> (
        match acc with
          | (prev_offset, prev_chunk) :: acc_rest
            when prev_chunk.stream_idx = chunk.stream_idx ->
              let prev_end = prev_offset + prev_chunk.length in
              let chunk_end = dst_offset + chunk.length in
              let new_start = min prev_offset dst_offset in
              let new_end = max prev_end chunk_end in
              let prev_data =
                List.map
                  (fun (pos, p) -> (prev_offset - new_start + pos, p))
                  prev_chunk.data
              in
              let chunk_data =
                List.map
                  (fun (pos, p) -> (dst_offset - new_start + pos, p))
                  chunk.data
              in
              let merged =
                {
                  length = new_end - new_start;
                  stream_idx = chunk.stream_idx;
                  time_base = chunk.time_base;
                  data = sort_data (prev_data @ chunk_data);
                }
              in
              aux ((new_start, merged) :: acc_rest) rest
          | _ -> aux ((dst_offset, chunk) :: acc) rest)
  in
  List.map snd (aux [] chunks)

let blit ~copy (src : ('a, 'b) content) src_pos (dst : ('a, 'b) content) dst_pos
    len =
  dst.params <- src.params;
  let dst_stop = dst_pos + len in
  (* Keep dst chunks that don't overlap with the blit region, with their positions *)
  let _, dst_chunks_with_offset =
    List.fold_left
      (fun (pos, acc) chunk ->
        let chunk_end = pos + chunk.length in
        let new_pos = chunk_end in
        if chunk_end <= dst_pos || dst_stop <= pos then
          (new_pos, (pos, chunk) :: acc)
        else (new_pos, acc))
      (0, []) dst.chunks
  in
  (* Extract chunks from src and place at dst_pos *)
  let _, new_chunks_with_offset =
    List.fold_left
      (fun (pos, acc) chunk ->
        let chunk_end = pos + chunk.length in
        let new_pos = chunk_end in
        if chunk_end <= src_pos || src_pos + len <= pos then (new_pos, acc)
        else (
          let start = max 0 (src_pos - pos) in
          let stop = min chunk.length (src_pos + len - pos) in
          let new_len = stop - start in
          let new_data = sub_data ~copy chunk start new_len in
          let dst_offset = dst_pos + (pos + start - src_pos) in
          (new_pos, (dst_offset, new_data) :: acc)))
      (0, []) src.chunks
  in
  (* Combine all chunks and sort by position, then collapse *)
  let all_chunks =
    List.rev dst_chunks_with_offset @ List.rev new_chunks_with_offset
  in
  let sorted_chunks =
    List.sort (fun (p1, _) (p2, _) -> compare p1 p2) all_chunks
  in
  let collapsed = collapse_chunks sorted_chunks in
  dst.chunks <- collapsed
