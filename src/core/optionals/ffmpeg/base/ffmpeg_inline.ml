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

(** Shared plumbing for the inline `ffmpeg.encode.*`, `ffmpeg.decode.*` and
    bitstream filter operators: each of them animates its source in a child
    clock and turns its frames into content of another type. *)

(** Copy a frame's metadata and track marks into the generator the transform
    writes to. Track marks landing exactly at the frame boundary belong to the
    next frame, so they are dropped here. *)
let relay_metadata generator frame =
  List.iter
    (fun (pos, m) -> Generator.add_metadata ~pos generator m)
    (Frame.get_all_metadata frame);
  let size = Lazy.Mutexed.force Frame.size in
  List.iter
    (fun pos -> Generator.add_track_mark ~pos generator)
    (List.filter (fun pos -> pos < size) (Frame.track_marks frame))

(** Animate [source] in a child clock, writing what [mk_process_frame] produces
    into the producer's generator. The transform is built on first use and
    dropped on flush, so codec state never survives a track boundary. *)
let mk_producer ~stack ~name ~field ~input_frame_t ~mk_process_frame source =
  let current = ref None in
  let process_frame generator payload =
    let process =
      match !current with
        | Some process -> process
        | None ->
            let process = mk_process_frame generator in
            current := Some process;
            process
    in
    match payload with
      | `Frame frame ->
          relay_metadata generator frame;
          process (`Frame frame)
      | `Flush ->
          process `Flush;
          current := None
  in
  let child_frame_type =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.add field (Type.fresh input_frame_t) Frame.Fields.empty)
  in
  let producer =
    new Child_support.producer
      ~stack
      ~child_frame_type
        (* Inline transforms run at roughly real rate, in bursts. *)
      ~check_self_sync:false ~name source
  in
  producer#child#set_process_frame process_frame;
  producer
