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

(** Connect sources to FFmpeg filters. *)

exception Not_ready

type 'a _duration_converter = {
  idx : int64;
  time_base : Avutil.rational;
  converter : 'a Avutil.Frame.t Ffmpeg_utils.Duration.t;
}

let track_mark_metadata = "liquidsoap_track_mark"

(** Everything that differs between an audio and a video end of the graph.
    Without this the two sides are the same code written twice. *)
type ('a, 'params) media = {
  get_data :
    Content.data -> ('params, 'a Avutil.frame) Ffmpeg_content_base.content;
  lift_data :
    ('params, 'a Avutil.frame) Ffmpeg_content_base.content -> Content.data;
  lift_params : 'params -> Content.format;
  frame_params : 'a Avutil.frame -> 'params;
  context_params : 'a Avfilter.context -> 'params;
}

let audio_media =
  {
    get_data = Ffmpeg_raw_content.Audio.get_data;
    lift_data = Ffmpeg_raw_content.Audio.lift_data;
    lift_params = Ffmpeg_raw_content.Audio.lift_params;
    frame_params = Ffmpeg_raw_content.AudioSpecs.frame_params;
    context_params =
      (fun context ->
        {
          Ffmpeg_raw_content.AudioSpecs.channel_layout =
            Some (Avfilter.channel_layout context);
          sample_rate = Some (Avfilter.sample_rate context);
          sample_format = Some (Avfilter.sample_format context);
        });
  }

let video_media =
  {
    get_data = Ffmpeg_raw_content.Video.get_data;
    lift_data = Ffmpeg_raw_content.Video.lift_data;
    lift_params = Ffmpeg_raw_content.Video.lift_params;
    frame_params = Ffmpeg_raw_content.VideoSpecs.frame_params;
    context_params =
      (fun context ->
        {
          Ffmpeg_raw_content.VideoSpecs.width = Some (Avfilter.width context);
          height = Some (Avfilter.height context);
          pixel_format = Some (Avfilter.pixel_format context);
          pixel_aspect = Avfilter.pixel_aspect context;
        });
  }

(* Content holding data from more than one stream keeps a chunk per stream, so
   take them all: a frame that straddles a track boundary carries two. *)
let chunks content =
  List.filter_map
    (fun chunk ->
      match chunk.Ffmpeg_content_base.data with
        | [] -> None
        | data ->
            Some
              ( chunk.Ffmpeg_content_base.stream_idx,
                chunk.Ffmpeg_content_base.time_base,
                data ))
    content.Ffmpeg_content_base.chunks

class virtual ['a] duration_converter =
  object (self)
    method virtual log : Log.t
    val mutable duration_converter : 'a _duration_converter option = None
    val mutable last_duration : int64 option = None

    method convert_duration ~stream_idx ~convert_ts ~time_base frame =
      let duration_converter =
        match duration_converter with
          | Some { idx; time_base = converter_time_base; converter }
            when idx = stream_idx && time_base = converter_time_base ->
              converter
          | _ ->
              let last_ts, offset =
                match duration_converter with
                  | None -> (None, 0L)
                  | Some { idx; converter } ->
                      if idx = stream_idx then
                        self#log#important "Unexpected time_base change!";
                      let last_ts = Ffmpeg_utils.Duration.last_ts converter in
                      let frame_ts =
                        Option.value ~default:0L (Avutil.Frame.pts frame)
                      in
                      let position =
                        Int64.add
                          (Option.value ~default:0L last_ts)
                          (Option.value ~default:0L last_duration)
                      in
                      let offset = Int64.sub position frame_ts in
                      (last_ts, offset)
              in
              let converter =
                Ffmpeg_utils.Duration.init ~offset ?last_ts ~mode:`PTS
                  ~src:time_base ~convert_ts ~get_ts:Avutil.Frame.pts
                  ~set_ts:Avutil.Frame.set_pts
                  ~get_duration:Avutil.Frame.duration ()
              in
              duration_converter <-
                Some { idx = stream_idx; time_base; converter };
              converter
      in
      last_duration <- Avutil.Frame.duration frame;
      Ffmpeg_utils.Duration.push duration_converter frame

    (* [Duration] holds each frame back until the next one arrives, to work out
       how long it lasted, so the last frame of a stream only ever comes out
       here. *)
    method flush_duration =
      match duration_converter with
        | None -> []
        | Some { converter; _ } -> snd (Ffmpeg_utils.Duration.flush converter)
  end

class ['a, 'params] base_output ~media ~pass_metadata ~name ~frame_t ~field
  source =
  object (self)
    inherit
      Output.output
        ~clock:(Clock.create ~sync:`Passive ~id:name ())
        ~infallible:false ~register_telnet:false ~name
        ~output_kind:"ffmpeg.filter.input" (Lang.source source) true as super

    inherit ['a] duration_converter

    initializer
      Typing.(
        self#frame_type <: frame_t;
        source#frame_type <: self#frame_type)

    val mutable input : [ `Frame of 'a Avutil.frame | `Flush ] -> unit =
      fun _ -> ()

    method self_sync = source#self_sync
    method set_input fn = input <- fn
    val mutable init : 'a Avutil.frame -> unit = fun _ -> assert false
    method set_init v = init <- v
    method start = ()
    method stop = ()
    method! reset = ()
    val is_up = Atomic.make false
    method! can_generate_frame = Atomic.get is_up && super#can_generate_frame
    initializer self#on_wake_up (fun () -> Atomic.set is_up true)
    initializer self#on_sleep (fun () -> Atomic.set is_up false)

    method private raw_ffmpeg_content content =
      chunks ((media : ('a, 'params) media).get_data content)

    (* Frame metadata is the only channel avfilter gives us, so a liquidsoap
       frame's metadata and its track marks ride along on the first frame we
       push for it. Track marks travel whether or not metadata is passed: they
       are a property of the stream, not of the metadata. *)
    method private graph_metadata memo =
      let metadata =
        if pass_metadata then (
          match Frame.get_all_metadata memo with
            | (_, m) :: _ -> Frame.Metadata.to_list m
            | _ -> [])
        else []
      in
      if Frame.has_track_marks memo then (track_mark_metadata, "1") :: metadata
      else metadata

    method send_frame memo =
      match self#raw_ffmpeg_content (Frame.get memo field) with
        | [] -> ()
        | chunks ->
            (match chunks with
              | (_, _, (_, frame) :: _) :: _ -> init frame
              | _ -> ());
            let pending = ref (self#graph_metadata memo) in
            List.iter
              (fun (stream_idx, time_base, data) ->
                List.iter
                  (fun (_, frame) ->
                    match
                      self#convert_duration ~convert_ts:true ~stream_idx
                        ~time_base frame
                    with
                      | None -> ()
                      | Some (_, frames) ->
                          List.iter
                            (fun (_, frame) ->
                              (match !pending with
                                | [] -> ()
                                | metadata ->
                                    Avutil.Frame.set_metadata frame metadata;
                                    pending := []);
                              input (`Frame frame))
                            frames)
                  data)
              chunks

    (* Filters with internal delay only emit their tail once the graph sees end
       of file, so drain on the way out. *)
    method private flush_input =
      List.iter (fun (_, frame) -> input (`Frame frame)) self#flush_duration;
      input `Flush

    initializer self#on_sleep (fun () -> self#flush_input)
  end

(** From the script perspective, the operator sending data to a filter graph is
    an output. *)
let audio_output ~pass_metadata ~name ~frame_t ~field source =
  new base_output ~media:audio_media ~pass_metadata ~name ~frame_t ~field source

let video_output ~pass_metadata ~name ~frame_t ~field source =
  new base_output ~media:video_media ~pass_metadata ~name ~frame_t ~field source

class ['a, 'params] input_base ~media ~name ~field ~pass_metadata ~self_sync
  ~is_ready ~pull frame_t =
  let stream_idx = Ffmpeg_content_base.new_stream_idx () in
  object (self)
    inherit ['a] duration_converter
    inherit Source.source ~name ()
    initializer Typing.(self#frame_type <: frame_t)
    method effective_source = (self :> Source.source)
    method fallible = true
    method remaining = Generator.remaining self#buffer
    method abort_track = ()
    method private stream_idx = stream_idx
    val mutable output = None

    (* The sink knows the format the graph settled on; hand it to the content
       type the script was type-checked against. *)
    method set_output v =
      let media = (media : ('a, 'params) media) in
      (match Frame.Fields.find_opt field self#content_type with
        | None -> ()
        | Some format ->
            Content.merge format
              (media.lift_params (media.context_params v.Avfilter.context)));
      output <- Some v

    method private put_data ~length data =
      match data with
        | [] -> ()
        | (_, frame) :: _ ->
            let time_base =
              Avfilter.time_base (Option.get output).Avfilter.context
            in
            let chunk =
              { Ffmpeg_content_base.length; stream_idx; time_base; data }
            in
            Generator.put self#buffer field
              (media.lift_data
                 {
                   Ffmpeg_content_base.params = media.frame_params frame;
                   chunks = [chunk];
                 })

    method private metadata_timestamps ~time_base frame =
      let get_time d =
        string_of_float
          (Frame.seconds_of_main
             (Int64.to_int
                (Ffmpeg_utils.convert_time_base ~src:time_base
                   ~dst:(Ffmpeg_utils.liq_main_ticks_time_base ())
                   d)))
      in
      List.fold_left
        (fun result (label, fn) ->
          match fn frame with
            | None -> result
            | Some v -> ("lavfi.liq." ^ label, get_time v) :: result)
        []
        [
          ("pts", Avutil.Frame.pts);
          ("duration", Avutil.Frame.duration);
          ("best_effort_timestamp", Avutil.Frame.best_effort_timestamp);
        ]

    method private flush_buffer output =
      let time_base = Avfilter.(time_base output.context) in
      fun () ->
        let frame = output.Avfilter.handler () in
        match
          self#convert_duration ~convert_ts:false ~stream_idx ~time_base frame
        with
          | Some (length, frames) ->
              List.iter
                (fun (pos, frame) ->
                  let metadata = Avutil.Frame.metadata frame in
                  let pos = Generator.length self#buffer + pos in
                  (* Track marks come back whether or not metadata is passed:
                     dropping them would silently merge tracks. *)
                  if List.mem_assoc track_mark_metadata metadata then
                    Generator.add_track_mark ~pos self#buffer;
                  if pass_metadata then (
                    let m =
                      List.filter
                        (fun (k, _) -> k <> track_mark_metadata)
                        metadata
                    in
                    if m <> [] then
                      Generator.add_metadata ~pos self#buffer
                        (Frame.Metadata.from_list
                           (m @ self#metadata_timestamps ~time_base frame))))
                frames;
              self#put_data ~length frames
          | None -> ()

    method self_sync : Clock.self_sync = self_sync self

    method pull =
      try
        (* Init is driven by the pull. *)
        let output =
          while output = None do
            if not (is_ready ()) then raise Not_ready;
            pull ()
          done;
          Option.get output
        in
        let flush = self#flush_buffer output in
        let rec f () =
          match
            try
              while true do
                flush ()
              done;
              `Done
            with
              | Avutil.Error `Eagain -> `Again
              (* The graph reached end of file: nothing more will ever come out
                 of this sink. *)
              | Avutil.Error `Eof -> `Eof
          with
            | `Eof | `Done -> ()
            | `Again ->
                if
                  Generator.length self#buffer < Lazy.force Frame.size
                  && is_ready ()
                then (
                  pull ();
                  f ())
        in
        f ()
      with Not_ready -> ()

    method private can_generate_frame =
      Generator.length self#buffer >= Lazy.force Frame.size || is_ready ()

    method private generate_frame =
      let size = Lazy.force Frame.size in
      if Generator.length self#buffer < Lazy.force Frame.size then self#pull;
      Generator.slice self#buffer size
  end

let audio_input ~field ~pass_metadata ~self_sync ~is_ready ~pull frame_t =
  new input_base
    ~media:audio_media ~name:"ffmpeg.filter.audio.output" ~field ~pass_metadata
    ~self_sync ~is_ready ~pull frame_t

let video_input ~field ~pass_metadata ~self_sync ~is_ready ~pull frame_t =
  new input_base
    ~media:video_media ~name:"ffmpeg.filter.video.output" ~field ~pass_metadata
    ~self_sync ~is_ready ~pull frame_t
