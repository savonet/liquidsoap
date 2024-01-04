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

let max_remaining a b = if b = -1 || a = -1 then -1 else max a b

type 'a field = { position : int; weight : 'a; field : Frame.field }
type 'a track = { mutable fields : 'a field list; source : Source.source }

class virtual base ~name tracks =
  let sources = List.map (fun { source } -> source) tracks in
  let self_sync_type = Utils.self_sync_type sources in
  object (self)
    inherit Source.operator ~name sources

    method stype =
      if List.exists (fun s -> s#stype = `Infallible) sources then `Infallible
      else `Fallible

    method self_sync =
      (Lazy.force self_sync_type, List.exists (fun s -> snd s#self_sync) sources)

    method remaining =
      let f cur pos =
        match (cur, pos) with
          | -1, -1 -> -1
          | x, -1 | -1, x -> x
          | x, y -> max_remaining x y
      in
      List.fold_left f (-1)
        (List.map
           (fun s -> s#remaining)
           (List.filter (fun (s : Source.source) -> s#is_ready ()) sources))

    method abort_track = List.iter (fun s -> s#abort_track) sources

    method private _is_ready ?frame () =
      List.exists (fun s -> s#is_ready ?frame ()) sources

    method seek_source =
      match sources with [s] -> s#seek_source | _ -> (self :> Source.source)

    val mutable track_frames = []

    method private track_frame source =
      try List.assq source track_frames
      with Not_found ->
        let f = Frame.create source#content_type in
        track_frames <- (source, f) :: track_frames;
        f

    method private feed_track ~offset pos { source } =
      let tmp = self#track_frame source in
      let start = Frame.position tmp in
      let tmp_pos =
        if start <= offset then (
          source#get tmp;
          Frame.position tmp)
        else start
      in
      max pos tmp_pos

    method private feed ~offset tracks =
      List.fold_left (self#feed_track ~offset) 0 tracks

    (* For backward compatibility: set metadata from the first
       track effectively summed. This should be called after #feed *)
    method private set_metadata buf offset position =
      match
        List.fold_left
          (fun cur { fields; source } ->
            if not (source#is_ready ~frame:buf ()) then cur
            else
              List.fold_left
                (fun cur { position; _ } ->
                  match cur with
                    | None -> Some (source, position)
                    | Some (_, pos) when position < pos ->
                        Some (source, position)
                    | _ -> cur)
                cur fields)
          None tracks
      with
        | None -> ()
        | Some (source, _) ->
            let tmp = self#track_frame source in
            List.iter
              (fun (pos, m) ->
                if offset <= pos && pos <= position then
                  Frame.set_metadata buf pos m)
              (Frame.get_all_metadata tmp)

    initializer
      self#on_after_output (fun () ->
          List.iter (fun (_, frame) -> Frame.clear frame) track_frames)
  end

(** Add/mix several sources together.
  * If [renorm], renormalize the PCM channels. *)
class audio_add ~renorm ~power ~field tracks =
  object (self)
    inherit base ~name:"audio.add" tracks

    method private get_frame buf =
      let renorm = renorm () in
      let power = power () in
      let total_weight, tracks =
        List.fold_left
          (fun (total_weight, tracks) { source; fields } ->
            let source_weight, fields =
              List.fold_left
                (fun (source_weight, fields) field ->
                  let weight = field.weight () in
                  let weight = if power then weight *. weight else weight in
                  (source_weight +. weight, { field with weight } :: fields))
                (0., []) fields
            in
            ( total_weight +. source_weight,
              if source#is_ready ~frame:buf () then { source; fields } :: tracks
              else tracks ))
          (0., []) tracks
      in
      let total_weight = if power then sqrt total_weight else total_weight in
      let offset = Frame.position buf in
      let pos = self#feed ~offset tracks in
      assert (offset <= pos);
      let audio_offset = Frame.audio_of_main offset in
      let pcm = Content.Audio.get_data (Frame.get buf field) in
      Audio.clear pcm audio_offset (Audio.length pcm);
      List.iter
        (fun { source; fields } ->
          let tmp = self#track_frame source in
          List.iter
            (fun { field; weight } ->
              let track_pcm = Content.Audio.get_data (Frame.get tmp field) in
              let audio_len =
                Frame.audio_of_main (Frame.position tmp - offset)
              in
              let c = if renorm then weight /. total_weight else weight in
              if c <> 1. then Audio.amplify c track_pcm audio_offset audio_len;
              Audio.add pcm audio_offset track_pcm audio_offset audio_len)
            fields)
        tracks;
      Frame.add_break buf pos;
      self#set_metadata buf offset pos
  end

class video_add ~field ~add tracks =
  object (self)
    inherit base ~name:"video.add" tracks

    method private get_frame buf =
      let tracks =
        List.fold_left
          (fun tracks track ->
            if track.source#is_ready ~frame:buf () then track :: tracks
            else tracks)
          [] tracks
      in
      let offset = Frame.position buf in
      let pos = self#feed ~offset tracks in
      let vbuf = Content.Video.get_data (Frame.get buf field) in
      let ( ! ) = Frame.video_of_main in
      let tracks =
        List.fold_left
          (fun tracks { source; fields } ->
            let tmp = self#track_frame source in
            tracks
            @ List.map
                (fun { position; field } -> (position, field, tmp))
                fields)
          [] tracks
      in
      let tracks =
        List.sort (fun (p, _, _) (p', _, _) -> Stdlib.compare p p') tracks
      in
      List.iteri
        (fun rank (position, field, tmp) ->
          let vtmp = Content.Video.get_data (Frame.get tmp field) in
          for i = !offset to !pos - 1 do
            let img =
              if rank = 0 then Video.Canvas.get vtmp i
              else
                add position (Video.Canvas.get vtmp i) (Video.Canvas.get vbuf i)
            in
            Video.Canvas.set vbuf i img
          done)
        tracks;
      Frame.add_break buf pos;
      self#set_metadata buf offset pos
  end

let get_tracks ~mk_weight p =
  let track_values = Lang.to_list (List.assoc "" p) in
  List.fold_left
    (fun (tracks, position) v ->
      let field, s = Lang.to_track v in
      let weight = mk_weight v in
      let field = { position; weight; field } in
      let track =
        match List.find_opt (fun { source } -> s == source) tracks with
          | Some track ->
              track.fields <- track.fields @ [field];
              tracks
          | None -> tracks @ [{ source = s; fields = [field] }]
      in
      (track, position + 1))
    ([], 0) track_values

let add_proto =
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
  ]

let add_audio_tracks p =
  let tracks, _ =
    get_tracks
      ~mk_weight:(fun v ->
        try Lang.to_float_getter (Liquidsoap_lang.Value.invoke v "weight")
        with _ -> fun () -> 1.)
      p
  in
  let renorm = Lang.to_bool_getter (List.assoc "normalize" p) in
  let power = List.assoc "power" p |> Lang.to_bool_getter in
  let field = Frame.Fields.audio in
  (field, new audio_add ~renorm ~power ~field tracks)

let _ =
  let frame_t = Format_type.audio () in
  let track_t =
    Type.meth ~optional:true "weight" ([], Lang.getter_t Lang.float_t) frame_t
  in
  Lang.add_track_operator ~base:Modules.track_audio "add" ~category:`Audio
    ~descr:"Mix audio tracks with optional normalization."
    (("", Lang.list_t track_t, None, None) :: add_proto)
    ~return_t:frame_t add_audio_tracks

let add_video_tracks p =
  let tracks, _ = get_tracks ~mk_weight:(fun _ -> ()) p in
  let add _ = Video.Canvas.Image.add in
  let field = Frame.Fields.video in
  (field, new video_add ~add ~field tracks)

let _ =
  let frame_t = Format_type.video () in
  Lang.add_track_operator ~base:Modules.track_video "add" ~category:`Video
    ~descr:"Merge video tracks."
    [("", Lang.list_t frame_t, None, None)]
    ~return_t:frame_t add_video_tracks

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
  let frame_t = Format_type.video () in
  Lang.add_track_operator ~base:Modules.track_video "tile" ~category:`Video
    ~descr:"Tile video tracks."
    [
      ( "proportional",
        Lang.bool_t,
        Some (Lang.bool true),
        Some "Scale preserving the proportions." );
      ("", Lang.list_t frame_t, None, None);
    ]
    ~return_t:frame_t
    (fun p ->
      let tracks, total_tracks = get_tracks ~mk_weight:(fun _ -> ()) p in
      let proportional = Lang.to_bool (List.assoc "proportional" p) in
      let tp = tile_pos total_tracks in
      let scale = Video_converter.scaler () in
      let add n tmp buf =
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
      let field = Frame.Fields.video in
      (field, new video_add ~field ~add tracks))

let _ =
  let frame_t = Lang.internal_tracks_t () in
  Lang.add_operator "add" ~category:`Audio
    (("", Lang.list_t (Lang.source_t frame_t), None, None)
    :: ( "weights",
         Lang.(list_t (getter_t float_t)),
         Some (Lang.list []),
         Some
           "Relative weight of the sources in the sum. The empty list stands \
            for the homogeneous distribution. These are used as amplification \
            coefficients if we are not normalizing." )
    :: add_proto)
    ~descr:
      "Mix sources, with optional normalization. Only relay metadata from the \
       first available source. Track marks are dropped from all sources."
    ~return_t:frame_t
    (fun p ->
      let sources_val = List.assoc "" p in
      let sources = List.map Lang.to_source (Lang.to_list sources_val) in
      if sources = [] then (new Debug_sources.fail "add" :> Source.source)
      else (
        let weights = Lang.to_list (List.assoc "weights" p) in
        let content_type =
          Frame.Fields.bindings (List.hd sources)#content_type
        in
        let audio_tracks, video_tracks =
          List.fold_left
            (fun (audio_tracks, video_tracks) (field, format) ->
              if Content_audio.is_format format then (
                let tracks = List.mapi (fun pos s -> (pos, s)) sources in
                ((field, tracks, fun s -> s) :: audio_tracks, video_tracks))
              else if Content_pcm_s16.is_format format then (
                let from_s s =
                  (Track_map.(
                     new track_map
                       ~name:"track.decode.audio.pcm_s16" ~field
                       ~fn:from_pcm_s16 s)
                    :> Source.source)
                in
                let to_s s =
                  (Track_map.(
                     new track_map
                       ~name:"track.encode.audio.pcm_s16" ~field ~fn:to_pcm_s16
                       s)
                    :> Source.source)
                in
                let tracks = List.mapi (fun pos s -> (pos, from_s s)) sources in
                ((field, tracks, to_s) :: audio_tracks, video_tracks))
              else if Content_pcm_f32.is_format format then (
                let from_s s =
                  (Track_map.(
                     new track_map
                       ~name:"track.decode.audio.pcm_s16" ~field
                       ~fn:from_pcm_f32 s)
                    :> Source.source)
                in
                let to_s s =
                  (Track_map.(
                     new track_map
                       ~name:"track.encode.audio.pcm_s16" ~field ~fn:to_pcm_f32
                       s)
                    :> Source.source)
                in
                let tracks = List.mapi (fun pos s -> (pos, from_s s)) sources in
                ((field, tracks, to_s) :: audio_tracks, video_tracks))
              else if Content_video.is_format format then
                (audio_tracks, (field, sources) :: video_tracks)
              else if
                Content_timed.Metadata.is_format format
                || Content_timed.Track_marks.is_format format
              then (audio_tracks, video_tracks)
              else
                raise (Error.Invalid_value (sources_val, "Invalid source type")))
            ([], []) content_type
        in
        let audio_tracks =
          List.map
            (fun (field, sources, to_source) ->
              let tracks =
                List.map
                  (fun (pos, s) ->
                    let track = Lang.track (field, s) in
                    try
                      let weight = List.nth weights pos in
                      Lang.meth track [("weight", weight)]
                    with _ -> track)
                  sources
              in
              let p =
                [
                  ("normalize", List.assoc "normalize" p);
                  ("power", List.assoc "power" p);
                  ("", Lang.list tracks);
                ]
              in
              let _, track = add_audio_tracks p in
              (field, to_source track))
            audio_tracks
        in
        let video_tracks =
          List.map
            (fun (field, sources) ->
              let tracks =
                Lang.list (List.map (fun s -> Lang.track (field, s)) sources)
              in
              let p = [("", tracks)] in
              let _, track = add_video_tracks p in
              (field, track))
            video_tracks
        in
        let tracks = audio_tracks @ video_tracks in
        let tracks = (Frame.Fields.metadata, snd (List.hd tracks)) :: tracks in
        let tracks =
          List.map
            (fun (field, track) ->
              let track = Lang.track (field, track) in
              (Frame.Fields.string_of_field field, track))
            tracks
        in
        let p = [("", Lang.meth Lang.unit tracks)] in
        (Muxer.muxer_operator p :> Source.source)))
