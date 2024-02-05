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

open Mm

(** Play multiple sources at the same time, and perform weighted mix *)

open Source

let max a b = if b = -1 || a = -1 then -1 else max a b

(** Add/mix several sources together.
  * If [renorm], renormalize the PCM channels.
  * The [video_init] (resp. [video_loop]) parameter is used to pre-process
  * the first layer (resp. next layers) in the sum; this generalization
  * is used to add either as an overlay or as a tiling. *)
class add ~renorm ~power (sources : ((unit -> float) * source) list) video_init
  video_loop =
  let self_sync_type = Utils.self_sync_type (List.map snd sources) in
  object (self)
    inherit operator ~name:"add" (List.map snd sources) as super

    (* We want the sources at the beginning of the list to
     * have their metadatas copied to the output stream, so direction
     * matters. The algo in get_frame reverses the list in the fold_left. *)
    val sources = List.rev sources

    method stype =
      if List.exists (fun (_, s) -> s#stype = `Infallible) sources then
        `Infallible
      else `Fallible

    method self_sync =
      ( Lazy.force self_sync_type,
        List.exists (fun (_, s) -> snd s#self_sync) sources )

    method remaining =
      let f cur pos =
        match (cur, pos) with
          | -1, -1 -> -1
          | x, -1 | -1, x -> x
          | x, y -> max x y
      in
      List.fold_left f (-1)
        (List.map
           (fun (_, s) -> s#remaining)
           (List.filter (fun (_, s) -> s#is_ready ?frame:None ()) sources))

    method abort_track = List.iter (fun (_, s) -> s#abort_track) sources

    method _is_ready ?frame () =
      List.exists (fun (_, s) -> s#is_ready ?frame ()) sources

    method seek_source =
      match sources with [(_, s)] -> s | _ -> (self :> Source.source)

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
    val mutable tmp = Frame.dummy ()

    method! wake_up a =
      super#wake_up a;
      tmp <- Frame.create self#content_type

    method private get_frame buf =
      let tmp = tmp in
      let renorm = renorm () in
      let sources = List.map (fun (w, s) -> (w (), s)) sources in
      let power = power () in
      (* Compute the list of ready sources, and their total weight. *)
      let weight, sources =
        List.fold_left
          (fun (t, l) (w, s) ->
            ( (w +. if power then t *. t else t),
              if s#is_ready ?frame:(Some buf) () then (w, s) :: l else l ))
          (0., []) sources
      in
      let weight = if power then sqrt weight else weight in
      (* Sum contributions. *)
      let breaks = AFrame.breaks buf in
      let offset = AFrame.position buf in
      let _, end_offset =
        List.fold_left
          (fun (rank, end_offset) (w, s) ->
            let buffer =
              (* The first source writes directly to [buf], the others write to
                 [tmp] and we'll sum that. *)
              if rank = 0 then buf
              else (
                Frame.clear tmp;
                Frame.set_breaks tmp breaks;
                tmp)
            in
            s#get buffer;
            let already = AFrame.position buffer in
            let c = if renorm then w /. weight else w in
            if c <> 1. then (
              try
                Audio.amplify c (AFrame.pcm buffer)
                  (Frame.audio_of_main offset)
                  (Frame.audio_of_main (already - offset))
              with Content.Invalid -> ());
            if rank > 0 then (
              (* The region grows, make sure it is clean before adding.
               * TODO the same should be done for video. *)
              (try
                 if already > end_offset then
                   Audio.clear (AFrame.pcm buf)
                     (Frame.audio_of_main end_offset)
                     (Frame.audio_of_main (already - end_offset));

                 (* Add to the main buffer. *)
                 Audio.add (AFrame.pcm buf) offset (AFrame.pcm tmp) offset
                   (already - offset)
               with Content.Invalid -> ());

              try
                let vbuf = VFrame.data buf in
                let vtmp = VFrame.data tmp in
                let ( ! ) = Frame.video_of_main in
                for i = !offset to !already - 1 do
                  let img =
                    video_loop rank (Video.Canvas.get vtmp i)
                      (Video.Canvas.get vbuf i)
                  in
                  Video.Canvas.set vbuf i img
                done
              with Content.Invalid -> ())
            else (
              try
                let vbuf = VFrame.data buf in
                let ( ! ) = Frame.video_of_main in
                for i = !offset to !already - 1 do
                  (* TODO @smimram *)
                  ignore (video_init (Video.Canvas.get vbuf i))
                done
              with Content.Invalid -> ());
            (rank + 1, max end_offset already))
          (0, offset) sources
      in
      (* If the other sources have filled more than the first one, the end of
         track in buf gets overridden. *)
      match Frame.breaks buf with
        | pos :: breaks when pos < end_offset ->
            Frame.set_breaks buf (end_offset :: breaks)
        | _ -> ()
  end

let _ =
  let frame_t = Lang.internal_tracks_t () in
  Lang.add_operator "add" ~category:`Audio
    ~descr:
      "Mix sources, with optional normalization. Only relay metadata from the \
       first source that is effectively summed."
    [
      ( "normalize",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool true),
        Some
          "Divide by the sum of weights of ready sources (or by the number of \
           ready sources if weights are not specified)." );
      ( "power",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Perform constant-power normalization." );
      ( "weights",
        Lang.list_t (Lang.getter_t Lang.float_t),
        Some (Lang.list []),
        Some
          "Relative weight of the sources in the sum. The empty list stands \
           for the homogeneous distribution. These are used as amplification \
           coefficients if we are not normalizing." );
      ("", Lang.list_t (Lang.source_t frame_t), None, None);
    ]
    ~return_t:frame_t
    (fun p ->
      let sources = Lang.to_source_list (List.assoc "" p) in
      let weights =
        List.map Lang.to_float_getter (Lang.to_list (List.assoc "weights" p))
      in
      let weights =
        if weights = [] then List.init (List.length sources) (fun _ () -> 1.)
        else weights
      in
      let renorm = Lang.to_bool_getter (List.assoc "normalize" p) in
      let power = List.assoc "power" p |> Lang.to_bool_getter in
      if List.length weights <> List.length sources then
        raise
          (Error.Invalid_value
             ( List.assoc "weights" p,
               "there should be as many weights as sources" ));
      (new add
         ~renorm ~power
         (List.map2 (fun w s -> (w, s)) weights sources)
         (fun _ -> ())
         (fun _ tmp buf -> Video.Canvas.Image.add tmp buf)
        :> Source.source))

let tile_pos n =
  let vert l x y x' y' =
    if l = 0 then [||]
    else (
      let dx = (x' - x) / l in
      let x = ref (x - dx) in
      Array.init l (fun _ ->
          x := !x + dx;
          (!x, y, dx, y' - y)))
  in
  let x' = Lazy.force Frame.video_width in
  let y' = Lazy.force Frame.video_height in
  let horiz m n =
    Array.append (vert m 0 0 x' (y' / 2)) (vert n 0 (y' / 2) x' y')
  in
  horiz (n / 2) (n - (n / 2))

let _ =
  let frame_t =
    Lang.frame_t (Lang.univ_t ())
      (Frame.Fields.make ~video:(Format_type.video ()) ())
  in
  Lang.add_operator ~base:Modules.video "tile" ~category:`Video
    ~descr:"Tile sources (same as add but produces tiles of videos)."
    [
      ("normalize", Lang.getter_t Lang.bool_t, Some (Lang.bool true), None);
      ( "power",
        Lang.getter_t Lang.bool_t,
        Some (Lang.bool false),
        Some "Perform constant-power normalization." );
      ( "weights",
        Lang.list_t (Lang.getter_t Lang.float_t),
        Some (Lang.list []),
        Some
          "Relative weight of the sources in the sum. The empty list stands \
           for the homogeneous distribution." );
      ( "proportional",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Scale preserving the proportions." );
      ("", Lang.list_t (Lang.source_t frame_t), None, None);
    ]
    ~return_t:frame_t
    (fun p ->
      let sources = Lang.to_source_list (List.assoc "" p) in
      let weights =
        List.map Lang.to_float_getter (Lang.to_list (List.assoc "weights" p))
      in
      let weights =
        if weights = [] then List.init (List.length sources) (fun _ () -> 1.)
        else weights
      in
      let renorm = Lang.to_bool_getter (List.assoc "normalize" p) in
      let power = List.assoc "power" p |> Lang.to_bool_getter in
      let proportional = Lang.to_bool (List.assoc "proportional" p) in
      let tp = tile_pos (List.length sources) in
      let scale = Video_converter.scaler () in
      let video_loop n buf tmp =
        let x, y, w, h = tp.(n) in
        let x, y, w, h =
          if proportional then (
            let sw, sh =
              (Video.Canvas.Image.width buf, Video.Canvas.Image.height buf)
            in
            if w * sh < sw * h then (
              let h' = sh * w / sw in
              (x, y + ((h - h') / 2), w, h'))
            else (
              let w' = sw * h / sh in
              (x + ((w - w') / 2), y, w', h)))
          else (x, y, w, h)
        in
        let tmp = Video.Canvas.Image.render tmp in
        let tmp' = Video.Image.create w h in
        scale tmp tmp';
        let tmp' =
          Video.Canvas.Image.make ~x ~y ~width:(-1) ~height:(-1) tmp'
        in
        Video.Canvas.Image.add tmp' buf
      in
      let video_init buf = video_loop 0 buf buf in
      if List.length weights <> List.length sources then
        raise
          (Error.Invalid_value
             ( List.assoc "weights" p,
               "there should be as many weights as sources" ));
      (new add
         ~renorm ~power
         (List.map2 (fun w s -> (w, s)) weights sources)
         video_init video_loop
        :> Source.source))
