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

open Frame_base

(* Meta content for frame. Should never be used for content resolution,
   hence the [assert false] below. *)
module Specs = struct
  type 'a frame_content = { breaks : 'a; metadata : 'a; media : 'a fields }
  type kind = [ `Frame_content ]
  type params = Content_base.format frame_content
  type data = Content_base.data frame_content

  let map fn { breaks; metadata; media = { audio; video; midi } } =
    {
      breaks = fn breaks;
      metadata = fn metadata;
      media = { audio = fn audio; video = fn video; midi = fn midi };
    }

  let make ~size = map (Content_base.make ~size)

  let blit src src_pos dst dst_pos len =
    Content_base.blit src.breaks src_pos dst.breaks dst_pos len;
    Content_base.blit src.metadata src_pos dst.metadata dst_pos len;
    Content_base.blit src.media.audio src_pos dst.media.audio dst_pos len;
    Content_base.blit src.media.video src_pos dst.media.video dst_pos len;
    Content_base.blit src.media.midi src_pos dst.media.midi dst_pos len

  let length d =
    assert (Content_base.(length d.breaks = length d.metadata));
    assert (Content_base.(length d.metadata = length d.media.audio));
    assert (Content_base.(length d.media.audio = length d.media.video));
    assert (Content_base.(length d.media.video = length d.media.midi));
    Content_base.length d.breaks

  let copy = map Content_base.copy
  let clear d = ignore (map Content_base.clear d)
  let params = map Content_base.format

  let merge p p' =
    ignore (Content_base.merge p.breaks p'.breaks);
    ignore (Content_base.merge p.metadata p'.metadata);
    ignore (Content_base.merge p.media.audio p'.media.audio);
    ignore (Content_base.merge p.media.video p'.media.video);
    ignore (Content_base.merge p.media.midi p'.media.midi);
    p

  let compatible p p' =
    Content_base.compatible p.breaks p'.breaks
    && Content_base.compatible p.metadata p'.metadata
    && Content_base.compatible p.media.audio p'.media.audio
    && Content_base.compatible p.media.video p'.media.video
    && Content_base.compatible p.media.midi p'.media.midi

  let string_of_params { media = { audio; video; midi } } =
    Printf.sprintf "frame_content(audio=%s,video=%s,midi=%s)"
      (Content_base.string_of_format audio)
      (Content_base.string_of_format video)
      (Content_base.string_of_format midi)

  let parse_param _ _ = None
  let kind = `Frame_content
  let default_params _ = assert false
  let string_of_kind _ = "frame_content"
  let kind_of_string _ = None
end

module Frame = struct
  include Specs
  include Content_base.MkContent (Specs)

  let lift_params media =
    lift_params
      {
        breaks = Content_timed.Breaks.lift_params ();
        metadata = Content_timed.Metadata.lift_params ();
        media;
      }

  let lift_data data =
    ignore (Specs.length data);
    lift_data data

  let blit_media src src_pos dst dst_pos len =
    let src = get_data src in
    let dst = get_data dst in
    Content_base.blit src.media.audio src_pos dst.media.audio dst_pos len;
    Content_base.blit src.media.video src_pos dst.media.video dst_pos len;
    Content_base.blit src.media.midi src_pos dst.media.midi dst_pos len

  let consolidate_chunks d =
    let params = get_params (Content_base.format d) in
    let size = Content_base.length d in
    let d = get_chunked_data d in
    let breaks, metadata, audio, video, midi =
      List.fold_left
        (fun (breaks, metadata, audio, video, midi)
             { Content_base.data; offset; size } ->
          let breaks = breaks @ [Content_base.sub data.breaks offset size] in
          let metadata =
            metadata @ [Content_base.sub data.metadata offset size]
          in
          let audio = audio @ [Content_base.sub data.media.audio offset size] in
          let video = video @ [Content_base.sub data.media.video offset size] in
          let midi = midi @ [Content_base.sub data.media.midi offset size] in
          (breaks, metadata, audio, video, midi))
        ([], [], [], [], []) d.Content_base.chunks
    in
    let concat ~make = function
      | [] -> make ~size:0
      | c :: l -> List.fold_left (fun c d -> Content_base.append c d) c l
    in
    d.Content_base.chunks <-
      [
        {
          Content_base.offset = 0;
          size;
          data =
            {
              breaks = concat ~make:(Content_base.make params.breaks) breaks;
              metadata =
                concat ~make:(Content_base.make params.metadata) metadata;
              media =
                {
                  audio =
                    concat ~make:(Content_base.make params.media.audio) audio;
                  video =
                    concat ~make:(Content_base.make params.media.video) video;
                  midi = concat ~make:(Content_base.make params.media.midi) midi;
                };
            };
        };
      ];
    d

  let get_audio d =
    let params = get_params (Content_base.format d) in
    match (consolidate_chunks d).Content_base.chunks with
      | [] -> Content_base.make ~size:0 params.media.audio
      | [{ Content_base.data = { media = { audio } } }] -> audio
      | _ -> assert false

  let set_audio d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content_base.chunks = 1);
    let { Content_base.data; size; offset } = List.hd d.Content_base.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with audio = c } } in
    d.Content_base.chunks <- [{ Content_base.size; offset; data }]

  let get_video d =
    let params = get_params (Content_base.format d) in
    match (consolidate_chunks d).Content_base.chunks with
      | [] -> Content_base.make ~size:0 params.media.video
      | [{ Content_base.data = { media = { video } } }] -> video
      | _ -> assert false

  let set_video d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content_base.chunks = 1);
    let { Content_base.data; size; offset } = List.hd d.Content_base.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with video = c } } in
    d.Content_base.chunks <- [{ Content_base.size; offset; data }]

  let get_midi d =
    let params = get_params (Content_base.format d) in
    match (consolidate_chunks d).Content_base.chunks with
      | [] -> Content_base.make ~size:0 params.media.midi
      | [{ Content_base.data = { media = { midi } } }] -> midi
      | _ -> assert false

  let set_midi d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content_base.chunks = 1);
    let { Content_base.data; size; offset } = List.hd d.Content_base.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with midi = c } } in
    d.Content_base.chunks <- [{ Content_base.size; offset; data }]

  let get_breaks d =
    match (consolidate_chunks d).Content_base.chunks with
      | [] -> []
      | [{ Content_base.data = { breaks } }] ->
          Content_timed.Breaks.get_data breaks
      | _ -> assert false

  let set_breaks d b =
    let d = consolidate_chunks d in
    assert (List.length d.Content_base.chunks = 1);
    let { Content_base.data; size; offset } = List.hd d.Content_base.chunks in
    assert (offset = 0);
    let data = { data with breaks = Content_timed.Breaks.lift_data ~size b } in
    d.Content_base.chunks <- [{ Content_base.size; offset; data }]

  let add_break b br = set_breaks b (br :: get_breaks b)

  let get_all_metadata d =
    match (consolidate_chunks d).Content_base.chunks with
      | [] -> []
      | [{ Content_base.data = { metadata } }] ->
          Content_timed.Metadata.get_data metadata
      | _ -> assert false

  let set_all_metadata d m =
    let d = consolidate_chunks d in
    assert (List.length d.Content_base.chunks = 1);
    let { Content_base.data; size; offset } = List.hd d.Content_base.chunks in
    assert (offset = 0);
    let data =
      { data with metadata = Content_timed.Metadata.lift_data ~size m }
    in
    d.Content_base.chunks <- [{ Content_base.size; offset; data }]

  let set_metadata b t m = set_all_metadata b ((t, m) :: get_all_metadata b)

  let get_metadata b t =
    try Some (List.assoc t (get_all_metadata b)) with Not_found -> None

  let free_metadata b t =
    set_all_metadata b
      (List.filter (fun (tt, _) -> t <> tt) (get_all_metadata b))

  let free_all_metadata b = set_all_metadata b []
end
