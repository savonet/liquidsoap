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

open Frame_base
module Content = Liquidsoap_lang.Content

(* Meta content for frame. Should never be used for content resolution,
   hence the [assert false] below. *)
module Specs = struct
  type 'a frame_content = { breaks : 'a; metadata : 'a; media : 'a fields }
  type kind = [ `Content ]
  type params = Content.format frame_content
  type data = Content.data frame_content

  let internal_content_type = None

  let map fn { breaks; metadata; media = { audio; video; midi } } =
    {
      breaks = fn breaks;
      metadata = fn metadata;
      media = { audio = fn audio; video = fn video; midi = fn midi };
    }

  let make ~size = map (Content.make ~size)

  let blit src src_pos dst dst_pos len =
    Content.blit src.breaks src_pos dst.breaks dst_pos len;
    Content.blit src.metadata src_pos dst.metadata dst_pos len;
    Content.blit src.media.audio src_pos dst.media.audio dst_pos len;
    Content.blit src.media.video src_pos dst.media.video dst_pos len;
    Content.blit src.media.midi src_pos dst.media.midi dst_pos len

  let length d =
    assert (Content.(length d.breaks = length d.metadata));
    assert (Content.(length d.metadata = length d.media.audio));
    assert (Content.(length d.media.audio = length d.media.video));
    assert (Content.(length d.media.video = length d.media.midi));
    Content.length d.breaks

  let copy = map Content.copy
  let clear d = ignore (map Content.clear d)
  let params = map Content.format

  let merge p p' =
    ignore (Content.merge p.breaks p'.breaks);
    ignore (Content.merge p.metadata p'.metadata);
    ignore (Content.merge p.media.audio p'.media.audio);
    ignore (Content.merge p.media.video p'.media.video);
    ignore (Content.merge p.media.midi p'.media.midi);
    p

  let compatible p p' =
    Content.compatible p.breaks p'.breaks
    && Content.compatible p.metadata p'.metadata
    && Content.compatible p.media.audio p'.media.audio
    && Content.compatible p.media.video p'.media.video
    && Content.compatible p.media.midi p'.media.midi

  let string_of_params { media = { audio; video; midi } } =
    Printf.sprintf "frame_content(audio=%s,video=%s,midi=%s)"
      (Content.string_of_format audio)
      (Content.string_of_format video)
      (Content.string_of_format midi)

  let parse_param _ _ = None
  let kind = `Content
  let default_params _ = assert false
  let string_of_kind _ = "frame_content"
  let kind_of_string _ = None
end

module Frame = struct
  include Specs
  include Content.MkContent (Specs)

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
    Content.blit src.media.audio src_pos dst.media.audio dst_pos len;
    Content.blit src.media.video src_pos dst.media.video dst_pos len;
    Content.blit src.media.midi src_pos dst.media.midi dst_pos len

  let consolidate_chunks d =
    let params = get_params (Content.format d) in
    let size = Content.length d in
    let d = get_chunked_data d in
    let breaks, metadata, audio, video, midi =
      List.fold_left
        (fun (breaks, metadata, audio, video, midi)
             { Content.data; offset; size } ->
          let breaks = breaks @ [Content.sub data.breaks offset size] in
          let metadata = metadata @ [Content.sub data.metadata offset size] in
          let audio = audio @ [Content.sub data.media.audio offset size] in
          let video = video @ [Content.sub data.media.video offset size] in
          let midi = midi @ [Content.sub data.media.midi offset size] in
          (breaks, metadata, audio, video, midi))
        ([], [], [], [], []) d.Content.chunks
    in
    let concat ~make = function
      | [] -> make ~size:0
      | c :: l -> List.fold_left (fun c d -> Content.append c d) c l
    in
    d.Content.chunks <-
      [
        {
          Content.offset = 0;
          size;
          data =
            {
              breaks = concat ~make:(Content.make params.breaks) breaks;
              metadata = concat ~make:(Content.make params.metadata) metadata;
              media =
                {
                  audio = concat ~make:(Content.make params.media.audio) audio;
                  video = concat ~make:(Content.make params.media.video) video;
                  midi = concat ~make:(Content.make params.media.midi) midi;
                };
            };
        };
      ];
    d

  let get_audio d =
    let params = get_params (Content.format d) in
    match (consolidate_chunks d).Content.chunks with
      | [] -> Content.make ~size:0 params.media.audio
      | [{ Content.data = { media = { audio } } }] -> audio
      | _ -> assert false

  let set_audio d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content.chunks = 1);
    let { Content.data; size; offset } = List.hd d.Content.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with audio = c } } in
    d.Content.chunks <- [{ Content.size; offset; data }]

  let get_video d =
    let params = get_params (Content.format d) in
    match (consolidate_chunks d).Content.chunks with
      | [] -> Content.make ~size:0 params.media.video
      | [{ Content.data = { media = { video } } }] -> video
      | _ -> assert false

  let set_video d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content.chunks = 1);
    let { Content.data; size; offset } = List.hd d.Content.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with video = c } } in
    d.Content.chunks <- [{ Content.size; offset; data }]

  let get_midi d =
    let params = get_params (Content.format d) in
    match (consolidate_chunks d).Content.chunks with
      | [] -> Content.make ~size:0 params.media.midi
      | [{ Content.data = { media = { midi } } }] -> midi
      | _ -> assert false

  let set_midi d c =
    let d = consolidate_chunks d in
    assert (List.length d.Content.chunks = 1);
    let { Content.data; size; offset } = List.hd d.Content.chunks in
    assert (offset = 0);
    let data = { data with media = { data.media with midi = c } } in
    d.Content.chunks <- [{ Content.size; offset; data }]

  let get_breaks d =
    match (consolidate_chunks d).Content.chunks with
      | [] -> []
      | [{ Content.data = { breaks } }] -> Content_timed.Breaks.get_data breaks
      | _ -> assert false

  let set_breaks d b =
    let d = consolidate_chunks d in
    assert (List.length d.Content.chunks = 1);
    let { Content.data; size; offset } = List.hd d.Content.chunks in
    assert (offset = 0);
    let data = { data with breaks = Content_timed.Breaks.lift_data ~size b } in
    d.Content.chunks <- [{ Content.size; offset; data }]

  let add_break b br = set_breaks b (br :: get_breaks b)

  let get_all_metadata d =
    match (consolidate_chunks d).Content.chunks with
      | [] -> []
      | [{ Content.data = { metadata } }] ->
          Content_timed.Metadata.get_data metadata
      | _ -> assert false

  let set_all_metadata d m =
    let d = consolidate_chunks d in
    assert (List.length d.Content.chunks = 1);
    let { Content.data; size; offset } = List.hd d.Content.chunks in
    assert (offset = 0);
    let data =
      { data with metadata = Content_timed.Metadata.lift_data ~size m }
    in
    d.Content.chunks <- [{ Content.size; offset; data }]

  let set_metadata b t m = set_all_metadata b ((t, m) :: get_all_metadata b)

  let get_metadata b t =
    try Some (List.assoc t (get_all_metadata b)) with Not_found -> None

  let free_metadata b t =
    set_all_metadata b
      (List.filter (fun (tt, _) -> t <> tt) (get_all_metadata b))

  let free_all_metadata b = set_all_metadata b []
end
