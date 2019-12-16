(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Play multiple sources at the same time, and perform weighted mix *)

open Source

let max a b = if b = -1 || a = -1 then -1 else max a b

(** Add/mix several sources together.
  * If [renorm], renormalize the PCM channels.
  * The [video_init] (resp. [video_loop]) parameter is used to pre-process
  * the first layer (resp. next layers) in the sum; this generalization
  * is used to add either as an overlay or as a tiling. *)
class add ~kind ~renorm (sources : (float * source) list) video_init video_loop
  =
  object
    inherit operator ~name:"add" kind (List.map snd sources)

    (* We want the sources at the beginning of the list to
     * have their metadatas copied to the output stream, so direction
     * matters. The algo in get_frame reverses the list in the fold_left. *)
    val sources = List.rev sources

    method stype =
      if List.exists (fun (_, s) -> s#stype = Infallible) sources then
        Infallible
      else Fallible

    method self_sync = List.exists (fun (_, s) -> s#self_sync) sources

    method remaining =
      List.fold_left max 0
        (List.map
           (fun (_, s) -> s#remaining)
           (List.filter (fun (_, s) -> s#is_ready) sources))

    method abort_track = List.iter (fun (_, s) -> s#abort_track) sources

    method is_ready = List.exists (fun (_, s) -> s#is_ready) sources

    method seek n = match sources with [(_, s)] -> s#seek n | _ -> 0

    (* We fill the buffer as much as possible, removing internal breaks.
     * Every ready source is asked for as much data as possible, by asking
     * it to fill the intermediate [tmp] buffer. Then that data is added
     * to the main buffer [buf], possibly with some amplitude change.
     *
     * The first source is asked to write directly on [buf], which avoids
     * copies when only one source is available -- a frequent situation.
     * Only the first available source's metadata is kept.
     *
     * Normally, all active sources are proposed to fill the buffer as much as
     * wanted, even if they end a track -- this is quite needed. There is an
     * exception when there is only one active source, then the end of tracks
     * are not hidden anymore, which is useful for transitions, for example. *)
    val tmp = Frame.create kind

    method private get_frame buf =
      (* Compute the list of ready sources, and their total weight *)
      let weight, sources =
        List.fold_left
          (fun (t, l) (w, s) -> (w +. t, if s#is_ready then (w, s) :: l else l))
          (0., []) sources
      in
      let weight = weight in
      (* Our sources are not allowed to have variable stream kinds.
       * This is necessary, because then we might not be able to sum them
       * if they vary in different ways.
       * The frame [buf] might be partially filled with completely different
       * content, but after the beginning of where we work there should always
       * be one type of data, hence the following helper. *)
      let fixed_content frame pos =
        let end_pos, c = Frame.content frame pos in
        assert (end_pos = Lazy.force Frame.size);
        c
      in
      (* Sum contributions *)
      let offset = Frame.position buf in
      let _, end_offset =
        List.fold_left
          (fun (rank, end_offset) (w, s) ->
            let buffer =
              (* The first source writes directly to [buf],
               * the others write to [tmp] and we'll sum that. *)
              if rank = 0 then buf
              else (
                Frame.clear tmp;
                Frame.set_breaks tmp [offset];
                tmp )
            in
            s#get buffer;
            let already = Frame.position buffer in
            let c = w /. weight in
            if c <> 1. && renorm then
              Audio.amplify c
                (Audio.sub (fixed_content buffer offset).Frame.audio
                   (Frame.audio_of_master offset)
                   (Frame.audio_of_master (already - offset)));
            if rank > 0 then (
              (* The region grows, make sure it is clean before adding.
               * TODO the same should be done for video. *)
              if already > end_offset then
                Audio.clear
                  (Audio.sub (fixed_content buf already).Frame.audio
                     (Frame.audio_of_master end_offset)
                     (Frame.audio_of_master (already - end_offset)));

              (* Add to the main buffer. *)
              Audio.add
                (Audio.sub (fixed_content buf offset).Frame.audio offset
                   (already - offset))
                (Audio.sub (fixed_content tmp offset).Frame.audio offset
                   (already - offset));
              let vbuf = (fixed_content buf offset).Frame.video in
              let vtmp = (fixed_content tmp offset).Frame.video in
              let ( ! ) = Frame.video_of_master in
              for c = 0 to Array.length vbuf - 1 do
                for i = !offset to !already - 1 do
                  video_loop rank (Video.get vbuf.(c) i) (Video.get vtmp.(c) i)
                done
              done )
            else (
              let vbuf = (fixed_content buf offset).Frame.video in
              let ( ! ) = Frame.video_of_master in
              for c = 0 to Array.length vbuf - 1 do
                for i = !offset to !already - 1 do
                  video_init (Video.get vbuf.(c) i)
                done
              done );
            (rank + 1, max end_offset already))
          (0, offset) sources
      in
      (* If the other sources have filled more than the first one,
       * the end of track in buf gets overriden. *)
      match Frame.breaks buf with
        | pos :: breaks when pos < end_offset ->
            Frame.set_breaks buf (end_offset :: breaks)
        | _ -> ()
  end

let () =
  (* TODO: add on midi chans also. *)
  let kind =
    Lang.Constrained
      {
        Frame.audio = Lang.Any_fixed 0;
        video = Lang.Any_fixed 0;
        midi = Lang.Fixed 0;
      }
  in
  let kind_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "add" ~category:Lang.SoundProcessing
    ~descr:
      "Mix sources, with optional normalization. Only relay metadata from the \
       first source that is effectively summed."
    [
      ("normalize", Lang.bool_t, Some (Lang.bool true), None);
      ( "weights",
        Lang.list_t Lang.float_t,
        Some (Lang.list ~t:Lang.int_t []),
        Some
          "Relative weight of the sources in the sum. The empty list stands \
           for the homogeneous distribution." );
      ("", Lang.list_t (Lang.source_t kind_t), None, None);
    ]
    ~kind
    (fun p kind ->
      let sources = Lang.to_source_list (List.assoc "" p) in
      let weights =
        List.map Lang.to_float (Lang.to_list (List.assoc "weights" p))
      in
      let weights =
        if weights = [] then Utils.make_list (List.length sources) 1.
        else weights
      in
      let renorm = Lang.to_bool (List.assoc "normalize" p) in
      if List.length weights <> List.length sources then
        raise
          (Lang_errors.Invalid_value
             ( List.assoc "weights" p,
               "there should be as many weights as sources" ));
      new add
        ~kind ~renorm
        (List.map2 (fun w s -> (w, s)) weights sources)
        (fun _ -> ())
        (fun _ buf tmp -> Video.Image.add tmp buf))

let tile_pos n =
  let vert l x y x' y' =
    if l = 0 then [||]
    else (
      let dx = (x' - x) / l in
      let x = ref (x - dx) in
      Array.init l (fun _ ->
          x := !x + dx;
          (!x, y, dx, y' - y)) )
  in
  let x' = Lazy.force Frame.video_width in
  let y' = Lazy.force Frame.video_height in
  let horiz m n =
    Array.append (vert m 0 0 x' (y' / 2)) (vert n 0 (y' / 2) x' y')
  in
  horiz (n / 2) (n - (n / 2))

let () =
  let kind = Lang.any_fixed_with ~video:1 () in
  let kind_t = Lang.kind_type_of_kind_format kind in
  Lang.add_operator "video.tile" ~category:Lang.VideoProcessing
    ~descr:"Tile sources (same as add but produces tiles of videos)."
    [
      ("normalize", Lang.bool_t, Some (Lang.bool true), None);
      ( "weights",
        Lang.list_t Lang.float_t,
        Some (Lang.list ~t:Lang.int_t []),
        Some
          "Relative weight of the sources in the sum. The empty list stands \
           for the homogeneous distribution." );
      ( "proportional",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Scale preserving the proportions." );
      ("", Lang.list_t (Lang.source_t kind_t), None, None);
    ]
    ~kind
    (fun p kind ->
      let sources = Lang.to_source_list (List.assoc "" p) in
      let weights =
        List.map Lang.to_float (Lang.to_list (List.assoc "weights" p))
      in
      let weights =
        if weights = [] then Utils.make_list (List.length sources) 1.
        else weights
      in
      let renorm = Lang.to_bool (List.assoc "normalize" p) in
      let proportional = Lang.to_bool (List.assoc "proportional" p) in
      let tp = tile_pos (List.length sources) in
      let scale = Video_converter.scaler () in
      let video_loop n buf tmp =
        let x, y, w, h = tp.(n) in
        let x, y, w, h =
          if proportional then (
            let sw, sh = (Video.Image.width buf, Video.Image.height buf) in
            if w * sh < sw * h then (
              let h' = sh * w / sw in
              (x, y + ((h - h') / 2), w, h') )
            else (
              let w' = sw * h / sh in
              (x + ((w - w') / 2), y, w', h) ) )
          else (x, y, w, h)
        in
        let tmp' = Video.Image.create w h in
        scale tmp tmp';
        Video.Image.add tmp' ~x ~y buf
      in
      let video_init buf = video_loop 0 buf buf in
      if List.length weights <> List.length sources then
        raise
          (Lang_errors.Invalid_value
             ( List.assoc "weights" p,
               "there should be as many weights as sources" ));
      new add
        ~kind ~renorm
        (List.map2 (fun w s -> (w, s)) weights sources)
        video_init video_loop)
