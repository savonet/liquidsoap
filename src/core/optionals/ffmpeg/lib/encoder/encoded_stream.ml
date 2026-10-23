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

module F = Liquidsoap_ffmpeg_formats.Encoder_format

let report (on_packet : key:bool -> time:int option -> unit)
    (packet : Av.encoded_packet) =
  on_packet ~key:packet.key
    ~time:
      (Option.bind packet.pts
         (Time_base.rescale_int64 ~src:packet.time_base ~dst:(Time_base.main ())))

type on_packet = key:bool -> time:int option -> unit

type t = {
  mk_stream : Frame.t -> unit;
  encode : position:int -> Frame.t -> unit;
  flush : unit -> unit;
  request_keyframe : int -> unit;
  codec_attr : unit -> string option;
  bitrate : unit -> int option;
  video_size : unit -> (int * int) option;
}

let error ?pos message = Lang_encoder.raise_error ~pos message

let rescale ~src ~dst v =
  match Time_base.rescale ~src ~dst v with
    | Some v -> v
    | None -> failwith "Timestamp out of range"

let per_unit n = { Avutil.num = 1; den = n }

let check_opts ?pos opts =
  match Opts.unused_message [("stream", opts)] with
    | Some message -> error ?pos message
    | None -> ()

(* encoder.md §3 step 1. *)
let stream_opts (opts : F.opts) =
  match List.assoc_opt "flags" opts with
    | Some (`String s)
      when List.mem "global_header"
             (List.concat_map (String.split_on_char '-')
                (String.split_on_char '+' s)) ->
        opts
    | _ -> Opts.add_flag opts ~name:"global_header" Avcodec.flag_global_header

(* ponytail: past the fault bound the output fails and restarts; encoder.md
   §5.2 rebuilds the codec instead, which needs a bindings call that replaces
   a stream's encoder. *)
let item_fault faults ~kind message =
  match Faults.item faults ~kind message with
    | `Item_fault -> ()
    | `Source_fault ->
        failwith (Printf.sprintf "%s: too many consecutive faults" kind)

let guarded faults ~kind f =
  try
    f ();
    Faults.delivered faults
  with Avutil.Error e -> item_fault faults ~kind (Avutil.string_of_error e)

let audio_runs frame field =
  match Frame.get frame field with
    | data ->
        List.map
          (fun (start, (run : Ffmpeg_content.Audio.Data.run)) ->
            (start, run.length, run.time_base, run.items))
          (Ffmpeg_content.Audio.get_data data).runs
    | exception Not_found -> []

let video_runs frame field =
  match Frame.get frame field with
    | data ->
        List.map
          (fun (start, (run : Ffmpeg_content.Video.Data.run)) ->
            (start, run.length, run.time_base, run.items))
          (Ffmpeg_content.Video.get_data data).runs
    | exception Not_found -> []

(* Positions of raw content not covered by a run are gaps (§3). *)
type coverage = { mutable covered : int; mutable gap : bool }

let iter_raw coverage ~position runs f =
  List.iter
    (fun (start, length, time_base, items) ->
      let start = position + start in
      if start > coverage.covered then coverage.gap <- true;
      coverage.covered <- start + length;
      List.iter (fun (p, item) -> f ~time_base ~abs:(start + p) item) items)
    runs

(* encoder-internal.md §2.1: what a raw track does not write comes from its
   first frame. *)
let first_raw_audio frame field =
  match Frame.get frame field with
    | data ->
        List.find_map
          (fun (_, (run : Ffmpeg_content.Audio.Data.run)) ->
            match run.items with (_, f) :: _ -> Some f | [] -> None)
          (Ffmpeg_content.Audio.get_data data).runs
    | exception _ -> None

let audio_layout ?pos (a : F.audio) =
  match a.channel_layout with
    | Some name -> (
        try Avutil.Channel_layout.find name
        with _ ->
          error ?pos (Printf.sprintf "Unknown channel layout: %s" name))
    | None -> (
        match Avutil.Channel_layout.get_default a.channels with
          | l when Avutil.Channel_layout.get_nb_channels l = a.channels -> l
          | _ | (exception _) ->
              error ?pos
                (Printf.sprintf "No default channel layout for %d channels"
                   a.channels))

type pcm_resampler = {
  convert : Content.data -> length:int -> Avutil.audio Avutil.frame;
  drain : unit -> Avutil.audio Avutil.frame;
}

let pcm_resampler ~pcm_kind ~in_layout ~in_rate ~out_layout ~out_format
    ~out_rate =
  let module Make
      (I : Swresample.AudioData)
      (G : sig
        val get : Content.data -> I.t
      end) =
  struct
    module R = Swresample.Make (I) (Swresample.Frame)

    let r =
      R.create in_layout in_rate out_layout ~out_sample_format:out_format
        out_rate

    let resampler =
      {
        convert = (fun data ~length -> R.convert ~length r (G.get data));
        drain = (fun () -> R.flush r);
      }
  end in
  match pcm_kind with
    | `Pcm ->
        let module M =
          Make
            (Swresample.PlanarFloatArray)
            (struct
              let get = Content.Audio.get_data
            end)
        in
        M.resampler
    | `Pcm_s16 ->
        let module M =
          Make
            (Swresample.S16PlanarBigArray)
            (struct
              let get = Content_pcm_s16.get_data
            end)
        in
        M.resampler
    | `Pcm_f32 ->
        let module M =
          Make
            (Swresample.FltPlanarBigArray)
            (struct
              let get = Content_pcm_f32.get_data
            end)
        in
        M.resampler

(* Samples of an overlapping raw frame are dropped from its front, through
   planar doubles and back. *)
let drop_front frame count =
  let format = Avutil.Audio.frame_format frame in
  let module In =
    Swresample.Make (Swresample.Frame) (Swresample.PlanarFloatArray)
  in
  let module Out =
    Swresample.Make (Swresample.PlanarFloatArray) (Swresample.Frame)
  in
  let layout = format.channel_layout and rate = format.sample_rate in
  let planes =
    In.convert
      (In.create layout ~in_sample_format:format.sample_format rate layout rate)
      frame
  in
  Out.convert
    (Out.create layout rate layout ~out_sample_format:format.sample_format rate)
    (Array.map (fun c -> Array.sub c count (Array.length c - count)) planes)

let audio ~(log : Log.t) ~pos ~field ~mode ~codec ~(a : F.audio) ~opts
    ~on_packet output =
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field field in
  let state = ref None in
  let resampler = ref None in
  let raw_stage = ref None in
  let coverage = { covered = 0; gap = false } in
  let next_main = ref 0 in
  (* encoder.md §13.2: the sample counter starts at the container's position,
     so a restarted HLS container keeps the variant's timeline. *)
  let started = ref false in
  let writer () = Option.get !state in
  let audio_stream = ref None in
  let feed frame = Audio_writer.feed (fst (writer ())) frame in
  let mk_stream frame =
    let first =
      match mode with `Raw -> first_raw_audio frame field | `Internal -> None
    in
    let inferred parameter =
      if List.mem parameter a.written then None else first
    in
    let rate =
      match inferred `Samplerate with
        | Some f -> (Avutil.Audio.frame_format f).sample_rate
        | None -> Lazy.Mutexed.force a.samplerate
    in
    let layout =
      match inferred `Channels with
        | Some f ->
            let layout = (Avutil.Audio.frame_format f).channel_layout in
            audio_layout ?pos
              { a with channels = Avutil.Channel_layout.get_nb_channels layout }
        | None -> audio_layout ?pos a
    in
    let requested =
      match a.sample_format with
        | Some name -> (
            try Avutil.Sample_format.find name
            with _ ->
              error ?pos (Printf.sprintf "Unknown sample format: %s" name))
        | None -> `Dbl
    in
    let sample_format = Avcodec.Audio.find_best_sample_format codec requested in
    let opts = Opts.of_list opts in
    let stream =
      try
        Av.new_audio_stream ~opts ~channel_layout:layout ~sample_rate:rate
          ~sample_format ~time_base:(per_unit rate) ~codec output
      with Avutil.Error e ->
        let message =
          Printf.sprintf "Cannot create %s stream (%d Hz, %s, %s): %s" kind rate
            (Avutil.Channel_layout.get_description layout)
            (Option.value ~default:"?"
               (Avutil.Sample_format.get_name sample_format))
            (Avutil.string_of_error e)
        in
        log#severe "%s" message;
        error ?pos message
    in
    check_opts ?pos opts;
    let frame_size =
      if List.mem `Variable_frame_size (Avcodec.capabilities codec) then None
      else (
        match Av.get_frame_size stream with n when n > 0 -> Some n | _ -> None)
    in
    let params =
      {
        Avfilter.Utils.sample_rate = rate;
        channel_layout = layout;
        sample_format;
      }
    in
    state :=
      Some
        ( Audio_writer.create ~frame_size ~params ~start:0
            (Av.write_frame ~on_packet:(report on_packet) stream),
          params );
    audio_stream := Some stream;
    match mode with
      | `Internal ->
          let in_layout = Avutil.Channel_layout.get_default a.channels in
          resampler :=
            Some
              (pcm_resampler ~pcm_kind:a.pcm_kind ~in_layout
                 ~in_rate:(Lazy.Mutexed.force Frame.audio_rate)
                 ~out_layout:layout ~out_format:sample_format ~out_rate:rate)
      | `Raw -> ()
  in
  let target () =
    let _, (params : Avfilter.Utils.audio_params) = writer () in
    {
      Avutil.Audio.sample_format = params.sample_format;
      sample_rate = params.sample_rate;
      channel_layout = params.channel_layout;
    }
  in
  let drain_raw_stage () =
    match !raw_stage with
      | Some (_, Some aformat) ->
          raw_stage := None;
          Helpers.Aformat.eof aformat feed
      | Some (_, None) -> raw_stage := None
      | None -> ()
  in
  let raw_stage_for format =
    match !raw_stage with
      | Some (key, stage) when Avutil.Audio.same_frame_format key format ->
          stage
      | previous ->
          drain_raw_stage ();
          let target = target () in
          let stage =
            if Avutil.Audio.same_frame_format format target then None
            else
              Some
                (Helpers.Aformat.create ~sample_format:target.sample_format
                   ~channel_layout:target.channel_layout
                   ~sample_rate:target.sample_rate
                   ~time_base:(per_unit format.sample_rate)
                   format)
          in
          if previous <> None then
            log#info "%s: audio conversion rebuilt for %s" kind
              (Pixel_formats.describe_audio format);
          raw_stage := Some (format, stage);
          stage
  in
  let encode_raw ~time_base:_ ~abs frame =
    let format = Avutil.Audio.frame_format frame in
    let samples = Avutil.Audio.frame_nb_samples frame in
    let main = Time_base.main () in
    if coverage.gap then (
      coverage.gap <- false;
      drain_raw_stage ();
      Audio_writer.jump
        (fst (writer ()))
        (rescale ~src:main ~dst:(per_unit (target ()).sample_rate) abs);
      next_main := abs);
    let overlap =
      if abs >= !next_main then 0
      else
        rescale ~src:main ~dst:(per_unit format.sample_rate) (!next_main - abs)
    in
    let frame =
      if overlap = 0 then Some frame
      else (
        item_fault faults ~kind
          (Printf.sprintf "%d overlapping samples dropped" (min overlap samples));
        if overlap >= samples then None else Some (drop_front frame overlap))
    in
    next_main :=
      max !next_main
        (abs + rescale ~src:(per_unit format.sample_rate) ~dst:main samples);
    Option.iter
      (fun frame ->
        guarded faults ~kind (fun () ->
            match raw_stage_for format with
              | None -> feed frame
              | Some aformat -> Helpers.Aformat.convert aformat feed frame))
      frame
  in
  let encode ~position frame =
    match mode with
      | `Internal ->
          let length = Frame.audio_of_main (Frame.position frame) in
          let r = Option.get !resampler in
          if not !started then (
            started := true;
            Audio_writer.jump
              (fst (writer ()))
              (rescale ~src:(Time_base.main ())
                 ~dst:(per_unit (target ()).sample_rate)
                 position));
          guarded faults ~kind (fun () ->
              feed (r.convert (Frame.get frame field) ~length))
      | `Raw -> iter_raw coverage ~position (audio_runs frame field) encode_raw
  in
  let flush () =
    if !state <> None then (
      guarded faults ~kind (fun () ->
          Option.iter (fun r -> feed (r.drain ())) !resampler;
          drain_raw_stage ());
      guarded faults ~kind (fun () -> Audio_writer.flush (fst (writer ()))))
  in
  let stream () = !audio_stream in
  {
    mk_stream;
    encode;
    flush;
    request_keyframe = (fun _ -> ());
    codec_attr = (fun () -> Option.bind (stream ()) Av.codec_attr);
    bitrate = (fun () -> Option.bind (stream ()) Av.bitrate);
    video_size = (fun () -> None);
  }

module Video_writer = struct
  type stage = { key : Avutil.Video.frame_format; fps : Helpers.Fps.t }

  type t = {
    log : Log.t;
    stream : (Avutil.output, Avutil.video, [ `Frame ]) Av.stream;
    frame_rate : int;
    mutable next_slot : int;
    mutable stage : stage option;
    on_packet : on_packet;
    mutable keyframe_slots : int list;
    mutable requested : int option;
  }

  let create ~log ~frame_rate ~on_packet stream =
    {
      log;
      stream;
      frame_rate;
      next_slot = 0;
      stage = None;
      on_packet;
      keyframe_slots = [];
      requested = None;
    }

  (* ffmpeg spec encoder.md §8.4: the first image at or after [time] is a key
     frame. *)
  let request_keyframe t time =
    if t.requested <> Some time then (
      t.requested <- Some time;
      let main_rate = Lazy.Mutexed.force Frame.main_rate in
      t.keyframe_slots <-
        t.keyframe_slots @ [((time * t.frame_rate) + main_rate - 1) / main_rate])

  let write t fps frame =
    let slot =
      match Avutil.Frame.pts frame with
        | Some pts -> (
            match
              Time_base.rescale_int64
                ~src:(Helpers.Fps.time_base fps)
                ~dst:(per_unit t.frame_rate) pts
            with
              | Some slot -> slot
              | None -> t.next_slot)
        | None -> t.next_slot
    in
    if slot >= t.next_slot then (
      Avutil.Frame.set_pts frame (Some (Int64.of_int slot));
      t.next_slot <- slot + 1;
      let due, later = List.partition (fun s -> s <= slot) t.keyframe_slots in
      t.keyframe_slots <- later;
      let frame =
        if due = [] then frame
        else (
          let frame = Avutil.Frame.dup frame in
          Avutil.Video.frame_force_keyframe frame;
          frame)
      in
      Av.write_frame ~on_packet:(report t.on_packet) t.stream frame)

  let drain t =
    match t.stage with
      | Some s ->
          t.stage <- None;
          Helpers.Fps.eof s.fps (write t s.fps)
      | None -> ()

  (* After a gap the grid continues at the first slot past the gap. *)
  let gap t ~abs =
    drain t;
    let main_rate = Lazy.Mutexed.force Frame.main_rate in
    t.next_slot <-
      max t.next_slot (((abs * t.frame_rate) + main_rate - 1) / main_rate)

  let push t ~abs ~duration frame =
    Avutil.Frame.set_pts frame (Some (Int64.of_int abs));
    Avutil.Frame.set_duration frame (Some (Int64.of_int duration));
    let format = Avutil.Video.frame_format frame in
    let stage =
      match t.stage with
        | Some s when Pixel_formats.same_video s.key format -> s
        | previous ->
            drain t;
            if previous <> None then
              t.log#info "Frame-rate conversion rebuilt for %s"
                (Pixel_formats.describe_video format);
            let fps =
              Helpers.Fps.create ~target:t.frame_rate
                ~start:(`Time (float t.next_slot /. float t.frame_rate))
                ~time_base:(Time_base.main ()) format
            in
            let s = { key = format; fps } in
            t.stage <- Some s;
            s
    in
    try Helpers.Fps.convert stage.fps (write t stage.fps) frame
    with exn ->
      t.stage <- None;
      raise exn
end

module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)

let video_frame () =
  rescale ~src:(Time_base.video ()) ~dst:(Time_base.main ()) 1

let global_side_data frame =
  List.filter
    (fun (raw : Avutil.Frame_side_data.raw) ->
      List.mem `Global (Avutil.Frame_side_data.props raw.kind))
    (Avutil.Frame.side_data frame)

let first_raw_side_data frame field =
  List.find_map
    (fun (_, _, _, items) ->
      List.find_map
        (fun (_, item) ->
          match global_side_data item with [] -> None | l -> Some l)
        items)
    (video_runs frame field)

let content_alpha ~mode frame field =
  match Frame.get frame field with
    | exception Not_found -> false
    | data -> (
        let format = Content.format data in
        try
          match mode with
            | `Internal -> Content.Video.alpha_of_format format
            | `Raw -> (
                match (Ffmpeg_content.Video.get_params format).pixel_format with
                  | Some p -> Pixel_formats.has_alpha p
                  | None -> false)
        with _ -> false)

let video ~(log : Log.t) ~pos ~field ~mode ~codec ~(v : F.video) ~opts
    ~on_packet output =
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field field in
  let frame_rate = Lazy.Mutexed.force v.framerate in
  let width = Lazy.Mutexed.force v.width in
  let height = Lazy.Mutexed.force v.height in
  let state = ref None in
  let scaler = ref None in
  let fit = ref None in
  let coverage = { covered = 0; gap = false } in
  let writer () = fst (Option.get !state) in
  let frame_pixel_format () = snd (Option.get !state) in
  let mk_stream frame =
    let alpha = v.alpha || content_alpha ~mode frame field in
    let target =
      try Pixel_formats.for_encoder ?configured:v.pixel_format ~alpha codec
      with Failure message -> error ?pos message
    in
    let hardware_pixel_format =
      Option.map
        (fun name ->
          try Avutil.Pixel_format.of_string name
          with _ ->
            error ?pos (Printf.sprintf "Unknown pixel format: %s" name))
        v.hwaccel_pixel_format
    in
    let opts = Opts.of_list opts in
    let hardware_context, pixel_format =
      try
        Hw_context.select ~log ~hwaccel:v.hwaccel ?hardware_pixel_format
          ?device:v.hwaccel_device ~opts ~target ~width ~height codec
      with Failure message -> error ?pos message
    in
    let side_data =
      match mode with
        | `Raw -> first_raw_side_data frame field
        | `Internal -> None
    in
    let stream =
      try
        Av.new_video_stream ~opts ?side_data
          ~frame_rate:{ Avutil.num = frame_rate; den = 1 }
          ?hardware_context ~pixel_format ~width ~height
          ~time_base:(per_unit frame_rate) ~codec output
      with Avutil.Error e ->
        let message =
          Printf.sprintf "Cannot create %s stream (%dx%d, %d fps): %s" kind
            width height frame_rate (Avutil.string_of_error e)
        in
        log#severe "%s" message;
        error ?pos message
    in
    check_opts ?pos opts;
    let frame_pixel_format =
      if Pixel_formats.is_hardware pixel_format then target else pixel_format
    in
    state :=
      Some
        ( Video_writer.create ~log ~frame_rate ~on_packet stream,
          frame_pixel_format )
  in
  let encode_image ~abs image =
    let gw, gh = Frame.video_dimensions () in
    let gw = Lazy.Mutexed.force gw and gh = Lazy.Mutexed.force gh in
    let alpha = Pixel_formats.has_alpha (frame_pixel_format ()) in
    let s =
      match !scaler with
        | Some s -> s
        | None ->
            let s =
              Scaler.create
                ~threads:(max 0 Settings.scaling_threads#get)
                [Pixel_formats.scaling_flag ()]
                gw gh
                (if alpha then `Yuva420p else `Yuv420p)
                width height (frame_pixel_format ())
            in
            scaler := Some s;
            s
    in
    let image =
      Mm.Video.Canvas.Image.render ~transparent:alpha
        (Mm.Video.Canvas.Image.viewport gw gh image)
    in
    if alpha then Mm.Image.YUV420.ensure_alpha image;
    Video_writer.push (writer ()) ~abs ~duration:(video_frame ())
      (Scaler.convert s (Image_pack.pack image))
  in
  let fit_frame frame =
    let pixel_format = frame_pixel_format () in
    if
      Avutil.Video.frame_get_width frame = width
      && Avutil.Video.frame_get_height frame = height
      && Avutil.Video.frame_get_pixel_format frame = pixel_format
    then [frame]
    else (
      let f =
        match !fit with
          | Some f -> f
          | None ->
              let f =
                Helpers.Fit.create
                  { width; height; pixel_format; pixel_aspect = None }
              in
              fit := Some f;
              f
      in
      Helpers.Fit.convert f frame)
  in
  let encode_raw ~time_base ~abs frame =
    let duration =
      match
        Option.bind
          (Avutil.Frame.duration frame)
          (Time_base.rescale_int64 ~src:time_base ~dst:(Time_base.main ()))
      with
        | Some d when d > 0 -> d
        | _ -> video_frame ()
    in
    if coverage.gap then (
      coverage.gap <- false;
      guarded faults ~kind (fun () -> Video_writer.gap (writer ()) ~abs));
    guarded faults ~kind (fun () ->
        List.iter
          (Video_writer.push (writer ()) ~abs ~duration)
          (fit_frame frame))
  in
  let encode ~position frame =
    match mode with
      | `Internal -> (
          match Frame.get frame field with
            | exception Not_found -> ()
            | data ->
                List.iter
                  (fun (p, image) ->
                    guarded faults ~kind (fun () ->
                        encode_image ~abs:(position + p) image))
                  (Content.Video.get_data data).data)
      | `Raw -> iter_raw coverage ~position (video_runs frame field) encode_raw
  in
  let flush () =
    if !state <> None then
      guarded faults ~kind (fun () -> Video_writer.drain (writer ()))
  in
  let stream () = Option.map (fun (w, _) -> w.Video_writer.stream) !state in
  {
    mk_stream;
    encode;
    flush;
    request_keyframe =
      (fun time ->
        Option.iter (fun (w, _) -> Video_writer.request_keyframe w time) !state);
    codec_attr = (fun () -> Option.bind (stream ()) Av.codec_attr);
    bitrate = (fun () -> Option.bind (stream ()) Av.bitrate);
    video_size = (fun () -> Option.map (fun _ -> (width, height)) (stream ()));
  }

let text_to_ass (s : F.subtitle) =
  match s.text_to_ass with
    | None -> F.default_text_to_ass
    | Some fn ->
        fun n text ->
          Lang.to_string
            (Lang.apply fn [("", Lang.int n); ("", Lang.string text)])

let subtitle ~(log : Log.t) ~pos ~field ~codec ~(s : F.subtitle) ~opts
    ~on_packet output =
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field field in
  let text_to_ass = text_to_ass s in
  let stream = ref None in
  let count = ref 0 in
  let last_stored = ref None in
  let micro = per_unit 1_000_000 and milli = per_unit 1000 in
  let mk_stream _ =
    let opts = Opts.of_list opts in
    (stream :=
       try Some (Av.new_subtitle_stream ~opts ~time_base:micro ~codec output)
       with Avutil.Error e ->
         let message =
           Printf.sprintf "Cannot create %s stream: %s" kind
             (Avutil.string_of_error e)
         in
         log#severe "%s" message;
         error ?pos message);
    check_opts ?pos opts
  in
  let write ~abs (sub : Subtitle_content.subtitle) =
    let main = Time_base.main () in
    let start_ms = rescale ~src:main ~dst:milli sub.start_time in
    let end_ms = rescale ~src:main ~dst:milli sub.end_time in
    let pts = rescale ~src:main ~dst:micro abs in
    let pts =
      match !last_stored with
        | Some last when pts + (start_ms * 1000) <= last ->
            last + 1 - (start_ms * 1000)
        | _ -> pts
    in
    last_stored := Some (pts + (start_ms * 1000));
    let ass =
      match sub.format with
        | `Ass -> sub.text
        | `Text -> text_to_ass !count sub.text
    in
    incr count;
    let content =
      {
        Avutil.Subtitle.format = 1;
        start_display_time = start_ms;
        end_display_time = end_ms;
        rectangles =
          [
            {
              pict = None;
              flags = (if sub.forced then [`Forced] else []);
              rect_type = `Ass;
              text = "";
              ass;
            };
          ];
        pts = Some (Int64.of_int pts);
      }
    in
    on_packet ~key:true ~time:(Some abs);
    Av.write_subtitle_frame (Option.get !stream)
      (Avutil.Subtitle.create_frame content)
  in
  let encode ~position frame =
    match Frame.get frame field with
      | exception Not_found -> ()
      | data ->
          List.iter
            (fun (p, sub) ->
              guarded faults ~kind (fun () -> write ~abs:(position + p) sub))
            (Subtitle_content.get_data data)
  in
  {
    mk_stream;
    encode;
    flush = (fun () -> ());
    request_keyframe = (fun _ -> ());
    codec_attr = (fun () -> Option.bind !stream Av.codec_attr);
    bitrate = (fun () -> Option.bind !stream Av.bitrate);
    video_size = (fun () -> None);
  }

let find_codec ?pos find name =
  try find name
  with _ ->
    error ?pos
      (Printf.sprintf
         "Cannot find %%ffmpeg encoder %s. Make sure that ffmpeg is compiled \
          with it!"
         name)

let create ~log ~pos ~field ~on_packet (encoded : F.encoded) output =
  let name =
    match encoded.codec with
      | Some name -> name
      | None ->
          error ?pos
            (Printf.sprintf "Track %s has no codec"
               (Frame.Fields.string_of_field field))
  in
  let opts = stream_opts encoded.opts in
  match encoded.options with
    | `Audio a ->
        let codec = find_codec ?pos Avcodec.Audio.find_encoder_by_name name in
        audio ~log ~pos ~field ~mode:encoded.mode ~codec ~a ~opts ~on_packet
          output
    | `Video v ->
        let codec = find_codec ?pos Avcodec.Video.find_encoder_by_name name in
        video ~log ~pos ~field ~mode:encoded.mode ~codec ~v ~opts ~on_packet
          output
    | `Subtitle s ->
        let codec =
          find_codec ?pos Avcodec.Subtitle.find_encoder_by_name name
        in
        subtitle ~log ~pos ~field ~codec ~s ~opts ~on_packet output
