(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

class merge_metadata tracks =
  let sources = List.map snd tracks in
  let stype =
    if List.for_all (fun s -> s#stype = `Infallible) sources then `Infallible
    else `Fallible
  in
  let self_sync_type = Utils.self_sync_type sources in
  object (self)
    inherit Source.operator ~name:"track.metadata.merge" sources as super
    method stype = stype

    method self_sync =
      (Lazy.force self_sync_type, List.exists (fun s -> snd s#self_sync) sources)

    method abort_track = List.iter (fun s -> s#abort_track) sources
    method private sources_ready = List.for_all (fun s -> s#is_ready) sources
    method is_ready = List.for_all (fun s -> s#is_ready) sources
    method seek len = len
    method remaining = -1
    val mutable track_frames = Hashtbl.create (List.length tracks)

    method private track_frame source =
      try Hashtbl.find track_frames source
      with Not_found ->
        let f = Frame.create source#content_type in
        Hashtbl.add track_frames source f;
        f

    method get_frame buf =
      let pos = Frame.position buf in
      let max_pos =
        List.fold_left
          (fun max_pos source ->
            let tmp_frame = self#track_frame source in
            if source#is_ready && Frame.is_partial tmp_frame then (
              source#get tmp_frame;
              List.iter
                (fun (p, m) ->
                  if pos <= p then (
                    match Frame.get_metadata buf p with
                      | None -> Frame.set_metadata buf p m
                      | Some m' -> Hashtbl.iter (Hashtbl.add m') m))
                (Frame.get_all_metadata tmp_frame));
            max max_pos (Frame.position tmp_frame))
          (Frame.position buf) sources
      in
      Frame.add_break buf max_pos

    method! advance =
      super#advance;
      Hashtbl.iter (fun _ frame -> Frame.clear frame) track_frames
  end

let _ =
  let metadata_t = Format_type.metadata in
  Lang.add_track_operator ~base:Muxer.track_metadata "merge" ~category:`Track
    ~descr:
      "Merge metadata from all given tracks. If two sources have metadata with \
       the same label at the same time, the one from the last source in the \
       list takes precedence."
    ~return_t:metadata_t
    [("", Lang.list_t metadata_t, None, None)]
    (fun p ->
      let tracks = List.map Lang.to_track (Lang.to_list (List.assoc "" p)) in
      (Frame.Fields.metadata, new merge_metadata tracks))
