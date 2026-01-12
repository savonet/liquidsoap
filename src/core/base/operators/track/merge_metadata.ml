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

class merge_metadata tracks =
  let sources = List.map snd tracks in
  let self_sync = Clock_base.self_sync sources in
  object (self)
    inherit Source.operator ~name:"track.metadata.merge" sources
    initializer Typing.(self#frame_type <: Lang.unit_t)
    method self_sync = self_sync ~source:self ()
    method fallible = false
    method abort_track = List.iter (fun s -> s#abort_track) sources
    method private ready_sources = List.filter (fun s -> s#is_ready) sources
    method private can_generate_frame = self#ready_sources <> []

    method effective_source =
      match self#ready_sources with
        | s :: [] -> s
        | _ -> (self :> Source.source)

    method remaining =
      match self#ready_sources with s :: [] -> s#remaining | _ -> -1

    method private generate_frame =
      match self#ready_sources with
        | [] -> assert false
        | s :: rest ->
            List.fold_left
              (fun frame source ->
                let l = Frame.get_all_metadata source#get_frame in
                let l =
                  List.fold_left
                    (fun l (pos, m) ->
                      ( pos,
                        Frame.Metadata.append
                          (Option.value ~default:Frame.Metadata.empty
                             (Frame.get_metadata frame pos))
                          m )
                      :: l)
                    [] l
                in
                Frame.add_all_metadata frame l)
              s#get_frame rest
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
