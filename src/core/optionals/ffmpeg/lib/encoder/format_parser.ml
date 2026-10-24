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

let error ?pos message = Lang_encoder.raise_error ~pos message
let pos = Value.pos

let integer v =
  let not_integral () = error ?pos:(pos v) "integer expected" in
  match v with
    | Value.Int { value } -> value
    | Value.Float { value } when Float.is_integer value -> int_of_float value
    | Value.String { value } -> (
        match Avutil.expr_parse_and_eval value with
          | f when Float.is_integer f -> int_of_float f
          | _ -> not_integral ()
          | exception _ -> not_integral ())
    | _ -> not_integral ()

let positive v =
  let n = integer v in
  if n < 1 then error ?pos:(pos v) "a positive integer is expected";
  n

let float v =
  match v with
    | Value.Int { value } -> float value
    | Value.Float { value } -> value
    | Value.String { value } -> (
        try Avutil.expr_parse_and_eval value
        with _ -> error ?pos:(pos v) "float expected")
    | _ -> error ?pos:(pos v) "float expected"

let string v =
  match v with
    | Value.Int { value } -> string_of_int value
    | Value.Float { value } -> Printf.sprintf "%f" value
    | Value.String { value } -> value
    | _ -> error ?pos:(pos v) "string expected"

let option_value v : F.value =
  match v with
    | Value.String { value } -> `String value
    | Value.Int { value } -> `Int value
    | Value.Float { value } -> `Float value
    | _ -> error ?pos:(pos v) "invalid option value"

let qscale opts v =
  let q = float v in
  F.set
    (Opts.add_flag opts ~name:"qscale" Avcodec.flag_qscale)
    "global_quality"
    (`Float (118. *. q))

let channels_of_layout v name =
  match Avutil.Channel_layout.find name with
    | l -> Avutil.Channel_layout.get_nb_channels l
    | exception _ ->
        error ?pos:(pos v) (Printf.sprintf "Unknown channel layout: %s" name)

let check_pixel_format v name =
  match Avutil.Pixel_format.of_string name with
    | f -> f
    | exception _ ->
        error ?pos:(pos v) (Printf.sprintf "Unknown pixel format: %s" name)

let has_alpha format =
  try List.mem `Alpha (Avutil.Pixel_format.descriptor format).flags
  with _ -> false

let writes parameter (a : F.audio) =
  if List.mem parameter a.written then a.written else parameter :: a.written

let audio_option (a : F.audio) = function
  | `Anonymous "pcm" -> Some { a with pcm_kind = `Pcm }
  | `Anonymous "pcm_s16" -> Some { a with pcm_kind = `Pcm_s16 }
  | `Anonymous "pcm_f32" -> Some { a with pcm_kind = `Pcm_f32 }
  | `Labelled ("channel_layout", v) ->
      let name =
        match v with
          | Value.Float { value } -> Printf.sprintf "%.1f" value
          | v -> string v
      in
      Some
        {
          a with
          channel_layout = Some name;
          channels = channels_of_layout v name;
          written = writes `Channels a;
        }
  | `Labelled (("channels" | "ac"), v) ->
      Some
        {
          a with
          channels = positive v;
          channel_layout = None;
          written = writes `Channels a;
        }
  | `Labelled (("samplerate" | "ar"), v) ->
      Some
        {
          a with
          samplerate = Lazy.Mutexed.from_val (positive v);
          written = writes `Samplerate a;
        }
  | `Labelled ("sample_format", v) ->
      Some { a with sample_format = Some (string v) }
  | _ -> None

let hwaccel v =
  match string v with
    | "auto" -> `Auto
    | "none" -> `None
    | "internal" -> `Internal
    | "device" -> `Device
    | "frame" -> `Frame
    | s -> error ?pos:(pos v) (Printf.sprintf "Invalid hwaccel value: %s" s)

let video_option (vf : F.video) = function
  | `Labelled (("framerate" | "r"), v) ->
      Some { vf with framerate = Lazy.Mutexed.from_val (positive v) }
  | `Labelled ("width", v) ->
      Some { vf with width = Lazy.Mutexed.from_val (positive v) }
  | `Labelled ("height", v) ->
      Some { vf with height = Lazy.Mutexed.from_val (positive v) }
  | `Labelled ("pixel_format", v) -> (
      match string v with
        | "guess" -> Some { vf with pixel_format = None; alpha = false }
        | name ->
            let format = check_pixel_format v name in
            Some { vf with pixel_format = Some name; alpha = has_alpha format })
  | `Labelled ("hwaccel", v) -> Some { vf with hwaccel = hwaccel v }
  | `Labelled ("hwaccel_device", v) -> (
      match string v with
        | "none" -> Some { vf with hwaccel_device = None }
        | d -> Some { vf with hwaccel_device = Some d })
  | `Labelled ("hwaccel_pixel_format", v) -> (
      match string v with
        | "guess" -> Some { vf with hwaccel_pixel_format = None }
        | name ->
            ignore (check_pixel_format v name);
            Some { vf with hwaccel_pixel_format = Some name })
  | _ -> None

let subtitle_option (_ : F.subtitle) = function
  | `Labelled ("text_to_ass", v) -> Some { F.text_to_ass = Some v }
  | _ -> None

let default_options : F.media -> F.options = function
  | `Audio ->
      `Audio
        {
          F.pcm_kind = `Pcm;
          channels = 2;
          channel_layout = None;
          samplerate =
            Lazy.Mutexed.from_fun (fun () ->
                Lazy.Mutexed.force Frame.audio_rate);
          sample_format = None;
          written = [];
        }
  | `Video ->
      let width, height = Frame.video_dimensions () in
      `Video
        {
          F.framerate =
            Lazy.Mutexed.from_fun (fun () ->
                Lazy.Mutexed.force Frame.video_rate);
          width;
          height;
          pixel_format = None;
          alpha = false;
          hwaccel = `Auto;
          hwaccel_device = None;
          hwaccel_pixel_format = None;
        }
  | `Subtitle -> `Subtitle { F.text_to_ass = None }

let media_option (options : F.options) param : F.options option =
  match options with
    | `Audio a -> Option.map (fun a -> `Audio a) (audio_option a param)
    | `Video v -> Option.map (fun v -> `Video v) (video_option v param)
    | `Subtitle s -> Option.map (fun s -> `Subtitle s) (subtitle_option s param)

let codec_value params =
  List.fold_left
    (fun codec -> function `Labelled ("codec", v) -> Some v | _ -> codec)
    None params

let encoded_track ~mode ~field params : F.track =
  let markers =
    List.filter_map
      (function `Anonymous s -> F.media_of_marker s | _ -> None)
      params
  in
  let codec_literal =
    match codec_value params with
      | Some (Value.String { value }) -> Some value
      | _ -> None
  in
  let media =
    match F.media_type ~markers ~field ~codec:codec_literal with
      | Some m -> m
      | None ->
          error
            ?pos:(Option.bind (codec_value params) pos)
            "Unable to find a track media content type. Please use one of the \
             available convention:\n\
             - Use `\"audio\"`, `\"video\"` or `\"subtitle\"` in the track \
             name, e.g. `%dolby_audio`\n\
             - Add `audio_content`, `video_content` or `subtitle_content` to \
             the track parameters, e.g. `%track(audio_content)`\n\
             - Use a static codec string, e.g. `%track(codec=\"libmp3lame\")`"
  in
  let codec, options, opts =
    List.fold_left
      (fun (codec, options, opts) param ->
        match param with
          | `Labelled ("codec", v) -> (Some (string v), options, opts)
          | `Labelled (("q" | "qscale"), v) -> (codec, options, qscale opts v)
          | `Anonymous s when F.media_of_marker s <> None ->
              (codec, options, opts)
          | _ -> (
              match media_option options param with
                | Some options -> (codec, options, opts)
                | None -> (
                    match param with
                      | `Labelled (k, v) ->
                          (codec, options, F.set opts k (option_value v))
                      | `Anonymous s ->
                          error (Printf.sprintf "Unknown track argument: %s" s)
                      | `Encoder (e, _) ->
                          error (Printf.sprintf "Unexpected argument: %%%s" e))))
      (None, default_options media, [])
      params
  in
  `Encode { F.mode; codec; options; opts }

let track (name, params) =
  let field, mode =
    try F.split_track_name name with Failure message -> error message
  in
  let track : F.track =
    match mode with
      | `Drop ->
          if params <> [] then
            error (Printf.sprintf "Track %%%s.drop takes no argument" field);
          `Drop
      | `Copy -> (
          match params with
            | [] | [`Anonymous "wait_for_keyframe"] -> `Copy `Wait_for_keyframe
            | [`Anonymous "ignore_keyframe"] -> `Copy `Ignore_keyframe
            | arg :: _ ->
                let pos, name =
                  match arg with
                    | `Anonymous s -> (None, s)
                    | `Labelled (l, v) -> (pos v, l)
                    | `Encoder (e, _) -> (None, "%" ^ e)
                in
                error ?pos
                  (Printf.sprintf "Invalid argument %s for a copy track" name))
      | (`Internal | `Raw) as mode -> encoded_track ~mode ~field params
  in
  (field, track)

let metadata v =
  try
    List.map
      (fun v ->
        match Lang.to_product v with
          | k, v -> (Lang.to_string k, Lang.to_string v))
      (Lang.to_list v)
  with _ -> error ?pos:(pos v) "metadata must be a list of string pairs"

let parse params =
  let format =
    {
      F.format = None;
      mime_type = None;
      output = `Stream;
      streams = [];
      interleaved = `Default;
      metadata = [];
      opts = [];
    }
  in
  let format =
    List.fold_left
      (fun (f : F.t) -> function
        | `Labelled ("format", Value.String { value = "none" }) ->
            { f with format = None }
        | `Labelled ("format", Value.String { value }) ->
            { f with format = Some value }
        | `Labelled ("metadata", v) -> { f with metadata = metadata v }
        | `Labelled ("interleaved", Value.Bool { value = true }) ->
            { f with interleaved = `True }
        | `Labelled ("interleaved", Value.Bool { value = false }) ->
            { f with interleaved = `False }
        | `Labelled ("interleaved", Value.String { value = "default" }) ->
            { f with interleaved = `Default }
        | `Labelled ("interleaved", v) ->
            error ?pos:(pos v) "Invalid interleaved value"
        | `Labelled (k, ((Value.String _ | Value.Int _ | Value.Float _) as v))
          ->
            { f with opts = F.set f.opts k (option_value v) }
        | `Labelled (k, v) ->
            error ?pos:(pos v) (Printf.sprintf "Invalid value for %s" k)
        | `Anonymous s -> error (Printf.sprintf "Unknown argument: %s" s)
        | `Encoder e ->
            let field, track = track e in
            if List.mem_assoc field f.streams then
              error
                (Printf.sprintf
                   "Field %s is used by more than one track of this format"
                   field);
            { f with streams = f.streams @ [(field, track)] })
      format params
  in
  Encoder.Ffmpeg format

let codec_media name : F.media option =
  let found f = match f name with _ -> true | exception _ -> false in
  if found Avcodec.Audio.find_encoder_by_name then Some `Audio
  else if found Avcodec.Video.find_encoder_by_name then Some `Video
  else if found Avcodec.Subtitle.find_encoder_by_name then Some `Subtitle
  else None

let () =
  F.lookups :=
    {
      F.authoritative = true;
      channels_of_layout =
        (fun name ->
          try
            Some
              (Avutil.Channel_layout.get_nb_channels
                 (Avutil.Channel_layout.find name))
          with _ -> None);
      pixel_format_has_alpha =
        (fun name ->
          try Some (has_alpha (Avutil.Pixel_format.of_string name))
          with _ -> None);
      codec_media;
    };
  Lang_encoder.register "ffmpeg" parse
