(*****************************************************************************
  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
type ('a, 'b) track = { mutable fields : 'a field list; data : 'b }

class virtual base ~name tracks =
  let sources = List.map (fun { data } -> data) tracks in
  let self_sync = Clock_base.self_sync sources in
  let infallible = List.exists (fun s -> not s#fallible) sources in
  object (self)
    inherit Source.operator ~name sources
    method fallible = not infallible
    method self_sync = self_sync ~source:self ()

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
           (List.filter (fun (s : Source.source) -> s#is_ready) sources))

    method abort_track = List.iter (fun s -> s#abort_track) sources

    method private can_generate_frame =
      List.exists (fun s -> s#is_ready) sources

    method private sources_ready = List.filter (fun s -> s#is_ready) sources

    method private tracks_ready =
      List.filter (fun { data = s } -> s#is_ready) tracks

    method private generate_frames =
      List.fold_left
        (fun frames { fields; data = source } ->
          let fields =
            List.map
              (fun ({ weight } as field) -> { field with weight = weight () })
              fields
          in
          let data =
            if source#is_ready then Some (source, source#get_frame) else None
          in
          { fields; data } :: frames)
        [] tracks

    method private frames_position frames =
      Option.value ~default:0
        (List.fold_left
           (fun pos { data } ->
             match (pos, data) with
               | _, None -> pos
               | None, Some (_, frame) -> Some (Frame.position frame)
               | Some p, Some (_, frame) -> Some (max p (Frame.position frame)))
           None frames)

    method seek_source =
      match sources with [s] -> s#seek_source | _ -> (self :> Source.source)

    (* For backward compatibility: set metadata from the first
       track effectively summed. *)
    method private set_metadata buf =
      match
        List.fold_left
          (fun cur { fields; data = source } ->
            List.fold_left
              (fun cur { position; _ } ->
                match cur with
                  | None -> Some (source, position)
                  | Some (_, pos) when position < pos -> Some (source, position)
                  | _ -> cur)
              cur fields)
          None self#tracks_ready
      with
        | None -> buf
        | Some (source, _) ->
            let metadata =
              Content.Metadata.get_data
                (Frame.Fields.find Frame.Fields.metadata source#get_frame)
            in
            Frame.set_all_metadata buf metadata
  end

(** Add/mix several sources together.
  * If [renorm], renormalize the PCM channels. *)
class audio_add ~renorm ~power ~field tracks =
  object (self)
    inherit base ~name:"audio.add" tracks

    method private generate_frame =
      let renorm = renorm () in
      let power = power () in
      let frames = self#generate_frames in
      let total_weight =
        List.fold_left
          (fun total_weight { fields } ->
            let source_weight =
              List.fold_left
                (fun source_weight { weight } ->
                  let weight = if power then weight *. weight else weight in
                  source_weight +. weight)
                0. fields
            in
            total_weight +. source_weight)
          0. frames
      in
      let total_weight = if power then sqrt total_weight else total_weight in
      let pos = self#frames_position frames in
      let buf = Frame.create ~length:pos self#content_type in
      let pcm = Content.Audio.get_data (Frame.get buf field) in
      Audio.clear pcm 0 (Audio.length pcm);
      List.iter
        (fun { data; fields } ->
          match data with
            | None -> ()
            | Some (_, frame) ->
                List.iter
                  (fun { field; weight } ->
                    let track_pcm =
                      Content.Audio.get_data (Frame.get frame field)
                    in
                    let c = if renorm then weight /. total_weight else weight in
                    let audio_len = Audio.length track_pcm in
                    if c <> 1. then Audio.amplify c track_pcm 0 audio_len;
                    Audio.add pcm 0 track_pcm 0 audio_len)
                  fields)
        frames;
      let buf = Frame.Fields.add field (Content.Audio.lift_data pcm) buf in
      self#set_metadata buf
  end

class video_add ~field ~add tracks =
  object (self)
    inherit base ~name:"video.add" tracks

    method private generate_frame =
      let frames = self#generate_frames in
      let length = self#frames_position frames in
      let frames =
        List.fold_left
          (fun frames { data; fields } ->
            match data with
              | None -> frames
              | Some (source, frame) ->
                  frames
                  @ List.map
                      (fun { position; field } ->
                        ( position,
                          source#last_image field,
                          Content.Video.get_data (Frame.get frame field) ))
                      fields)
          [] frames
      in
      let frames =
        List.sort (fun (p, _, _) (p', _, _) -> Stdlib.compare p p') frames
      in
      let create, frames =
        match frames with
          | [] ->
              ( (fun ~pos:_ ~width ~height () ->
                  Video.Canvas.Image.create width height),
                [] )
          | (_, last_image, data) :: rest ->
              ( (fun ~pos ~width:_ ~height:_ () ->
                  self#nearest_image ~pos ~last_image data),
                rest )
      in
      let buf = self#generate_video ~field ~create length in
      let data =
        List.map
          (fun (pos, img) ->
            ( pos,
              List.fold_left
                (fun img (rank, last_image, data) ->
                  add rank (self#nearest_image ~pos ~last_image data) img)
                img frames ))
          buf.Content.Video.data
      in
      let frame =
        Frame.set_data
          (Frame.create ~length Frame.Fields.empty)
          field Content.Video.lift_data
          { buf with Content.Video.data }
      in
      self#set_metadata frame
  end

let get_tracks ~mk_weight p =
  let track_values = Lang.to_list (List.assoc "" p) in
  List.fold_left
    (fun (tracks, position) v ->
      let field, s = Lang.to_track v in
      let weight = mk_weight v in
      let field = { position; weight; field } in
      let track =
        match List.find_opt (fun { data = source } -> s == source) tracks with
          | Some track ->
              track.fields <- track.fields @ [field];
              tracks
          | None -> tracks @ [{ data = s; fields = [field] }]
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
  let tracks, _ = get_tracks ~mk_weight:(fun _ () -> ()) p in
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
      let tracks, total_tracks = get_tracks ~mk_weight:(fun _ () -> ()) p in
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
        let track_sources = List.map (fun (_, s) -> s) tracks in
        let tracks =
          List.map
            (fun (field, track) ->
              let track = Lang.track (field, track) in
              (Frame.Fields.string_of_field field, track))
            tracks
        in
        let p = [("", Lang.meth Lang.unit tracks)] in
        let s = (Muxer.muxer_operator p :> Source.source) in
        (* Make sure all track positions are the same as the top-level source. *)
        List.iter
          (fun t -> Unifier.(t#stack_unifier <-- s#stack_unifier))
          track_sources;
        s))
