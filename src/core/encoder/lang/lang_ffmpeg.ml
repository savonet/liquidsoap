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

open Value
open Ground

type decode_type = [ `Raw | `Internal ]
type content_type = [ `Audio | `Video ]

type encoder_params =
  decode_type * content_type * (string * Liquidsoap_lang.Value.t) list

type mode =
  [ `Copy of Liquidsoap_lang.Value.t option | `Encode of encoder_params ]

type parsed_encoder = Frame.field * mode

let to_int t =
  match t.value with
    | Ground (Int i) -> i
    | Ground (String s) -> int_of_string s
    | Ground (Float f) -> int_of_float f
    | _ -> Lang_encoder.raise_error ~pos:t.pos "integer expected"

let to_string t =
  match t.value with
    | Ground (Int i) -> Printf.sprintf "%i" i
    | Ground (String s) -> s
    | Ground (Float f) -> Printf.sprintf "%f" f
    | _ -> Lang_encoder.raise_error ~pos:t.pos "string expected"

let to_float t =
  match t.value with
    | Ground (Int i) -> float i
    | Ground (String s) -> float_of_string s
    | Ground (Float f) -> f
    | _ -> Lang_encoder.raise_error ~pos:t.pos "float expected"

let to_copy_opt t =
  match t.value with
    | Ground (String "wait_for_keyframe") -> `Wait_for_keyframe
    | Ground (String "ignore_keyframe") -> `Ignore_keyframe
    | _ ->
        Lang_encoder.raise_error ~pos:t.pos
          ("Invalid value for copy encoder parameter: " ^ Value.to_string t)

let has_content ~to_static_string name p =
  List.exists (fun (lbl, v) -> lbl = "" && to_static_string v = Some name) p

(* The following conventions are used to
   infer media type from an encoder:
   - encoder has "audio" or "video" in its name,
     e.g. dolby_audio, video_1 etc.
   - encoder has "audio_content" or "video_content" in its arguments:
     %track(audio_content, ...) or %track(video_content, ...)
   - encoder has a static codec string, e.g.
     %track(codec="libmp3lame") *)
let stream_media_type ~to_pos ~to_static_string name args =
  let raise pos =
    Lang_encoder.raise_error ~pos
      {|Unable to find a track media content type. Please use one of the available convention:
- Use `"audio"` or `"video"` in the track name, e.g. `%dolby_audio`
- Add `audio_content` or `video_content` to the track parameters, e.g. `%track(audio_content)`
- Use a static codec string, e.g. `%track(codec="libmp3lame")`|}
  in
  match (name, args) with
    | _ when has_content ~to_static_string "audio_content" args -> `Audio
    | _ when has_content ~to_static_string "video_content" args -> `Video
    | _ when Pcre.pmatch ~pat:"audio" name -> `Audio
    | _ when Pcre.pmatch ~pat:"video" name -> `Video
    | _ -> (
        match List.assoc_opt "codec" args with
          | Some t -> (
              let codec = to_static_string t in
              try
                ignore (Avcodec.Audio.find_encoder_by_name (Option.get codec));
                `Audio
              with _ -> (
                try
                  ignore (Avcodec.Video.find_encoder_by_name (Option.get codec));
                  `Video
                with _ -> raise (to_pos t)))
          | None -> raise None)

let copy_param = function
  | [] -> None
  | [("", t)] -> Some t
  | [(l, v)] | _ :: (l, v) :: _ -> Lang_encoder.raise_generic_error (l, `Value v)

let parse_encoder_name name =
  let field, mode =
    match String.split_on_char '.' name with
      | [field] -> (field, "")
      | field :: mode :: _ -> (field, mode)
      | _ -> failwith "invalid content field"
  in
  let mode =
    match mode with
      | "copy" -> `Copy
      | "raw" -> `Raw
      | "" -> `Internal
      | _ -> failwith "invalid content field"
  in
  (field, mode)

let to_static_string_value = function
  | Value.{ value = Ground (Ground.String s) } -> Some s
  | _ -> None

let parse_encoder_params ~to_pos (name, p) : parsed_encoder =
  let field, mode = parse_encoder_name name in
  let mode =
    match mode with
      | `Copy -> `Copy (copy_param p)
      | `Raw ->
          `Encode
            ( `Raw,
              stream_media_type ~to_static_string:to_static_string_value ~to_pos
                name p,
              p )
      | `Internal ->
          `Encode
            ( `Internal,
              stream_media_type ~to_static_string:to_static_string_value ~to_pos
                name p,
              p )
  in
  let field = Frame.Fields.register field in
  (field, mode)

let to_static_string_term = function
  | `Term Term.{ term = Ground (Ground.String s) } -> Some s
  | _ -> None

let term_pos = function
  | `Term { Term.t = { Type.pos } } -> pos
  | _ -> raise Exit

let type_of_encoder =
  List.fold_left
    (fun content_type p ->
      match p with
        | "", `Encoder (name, args) ->
            let field, mode = parse_encoder_name name in
            let format =
              match mode with
                | `Copy ->
                    Content.(default_format (kind_of_string "ffmpeg.copy"))
                | `Raw -> (
                    match
                      stream_media_type ~to_pos:term_pos
                        ~to_static_string:to_static_string_term name args
                    with
                      | `Audio ->
                          Content.(
                            default_format (kind_of_string "ffmpeg.audio.raw"))
                      | `Video ->
                          Content.(
                            default_format (kind_of_string "ffmpeg.video.raw")))
                | `Internal -> (
                    match
                      stream_media_type ~to_pos:term_pos
                        ~to_static_string:to_static_string_term name args
                    with
                      | `Audio ->
                          let channels =
                            try
                              let name, channels =
                                try ("channels", List.assoc "channels" args)
                                with Not_found -> ("ac", List.assoc "ac" args)
                              in
                              match channels with
                                | `Term { Term.term = Term.Ground (Int n) } -> n
                                | `Term ({ t = { Type.pos } } as tm) ->
                                    Lang_encoder.raise_error ~pos
                                      (Printf.sprintf
                                         "Invalid value %s for %s parameter. \
                                          Only static numbers are allowed."
                                         name (Term.to_string tm))
                                | _ ->
                                    Lang_encoder.raise_error ~pos:None
                                      (Printf.sprintf
                                         "Invalid value for %s parameter." name)
                            with Not_found -> 2
                          in
                          let pcm_kind =
                            List.fold_left
                              (fun pcm_kind -> function
                                | ( "",
                                    `Term
                                      { Term.term = Term.Ground (String "pcm") }
                                  ) ->
                                    Content.Audio.kind
                                | ( "",
                                    `Term
                                      {
                                        Term.term =
                                          Term.Ground (String "pcm_s16");
                                      } ) ->
                                    Content_pcm_s16.kind
                                | ( "",
                                    `Term
                                      {
                                        Term.term =
                                          Term.Ground (String "pcm_f32");
                                      } ) ->
                                    Content_pcm_f32.kind
                                | _ -> pcm_kind)
                              Content.Audio.kind args
                          in
                          Frame_base.format_of_channels ~pcm_kind channels
                      | `Video -> Content.(default_format Video.kind))
            in
            let field = Frame.Fields.register field in
            Frame.Fields.add field
              (Type.make (Format_type.descr (`Format format)))
              content_type
        | _ -> content_type)
    Frame.Fields.empty

let flag_qscale = ref 0
let qp2lambda = ref 0

(* Looks like this is how ffmpeg CLI does it.
   See: https://github.com/FFmpeg/FFmpeg/blob/4782124b90cf915ede2cebd871be82fc0267a135/fftools/ffmpeg_opt.c#L1567-L1570 *)
let set_global_quality q opts =
  let flags =
    match Hashtbl.find_opt opts "flags" with
      | Some (`Int f) -> f
      | Some _ -> assert false
      | None -> 0
  in
  let flags = flags lor !flag_qscale in
  Hashtbl.replace opts "flags" (`Int flags);
  Hashtbl.replace opts "global_quality" (`Float (float !qp2lambda *. q))

let ffmpeg_gen params =
  let defaults =
    {
      Ffmpeg_format.format = None;
      output = `Stream;
      streams = [];
      opts = Hashtbl.create 0;
    }
  in

  let default_audio =
    {
      Ffmpeg_format.pcm_kind = Content_audio.kind;
      channels = 2;
      samplerate = Frame.audio_rate;
      sample_format = None;
    }
  in

  let default_video =
    {
      Ffmpeg_format.framerate = Frame.video_rate;
      width = Frame.video_width;
      height = Frame.video_height;
      pixel_format = None;
      hwaccel = `Auto;
      hwaccel_device = None;
    }
  in

  let parse_opts opts = function
    | "q", t | "qscale", t -> set_global_quality (to_float t) opts
    | "", { value = Ground (String "audio_content"); _ }
    | "", { value = Ground (String "video_content"); _ }
    | "codec", _ ->
        ()
    | k, { value = Ground (String s); _ } -> Hashtbl.add opts k (`String s)
    | k, { value = Ground (Int i); _ } -> Hashtbl.add opts k (`Int i)
    | k, { value = Ground (Float fl); _ } -> Hashtbl.add opts k (`Float fl)
    | _, t -> Lang_encoder.raise_error ~pos:t.pos "unexpected option"
  in

  let rec parse_audio_args ~opts options = function
    | [] -> options
    (* Audio options *)
    | ("", t) :: args when to_string t = "pcm" ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.pcm_kind = Content_audio.kind }
          args
    | ("", t) :: args when to_string t = "pcm_s16" ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.pcm_kind = Content_pcm_s16.kind }
          args
    | ("", t) :: args when to_string t = "pcm_f32" ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.pcm_kind = Content_pcm_f32.kind }
          args
    | ("channels", t) :: args ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.channels = to_int t }
          args
    | ("ac", t) :: args ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.channels = to_int t }
          args
    | ("samplerate", t) :: args ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.samplerate = Lazy.from_val (to_int t) }
          args
    | ("ar", t) :: args ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.samplerate = Lazy.from_val (to_int t) }
          args
    | ("sample_format", t) :: args ->
        parse_audio_args ~opts
          { options with Ffmpeg_format.sample_format = Some (to_string t) }
          args
    | arg :: args ->
        parse_opts opts arg;
        parse_audio_args ~opts options args
  in
  let rec parse_video_args ~opts options = function
    | [] -> options
    | ("framerate", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.framerate = Lazy.from_val (to_int t) }
          args
    | ("r", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.framerate = Lazy.from_val (to_int t) }
          args
    | ("width", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.width = Lazy.from_val (to_int t) }
          args
    | ("height", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.height = Lazy.from_val (to_int t) }
          args
    | ("pixel_format", { value = Ground (String "guess"); _ }) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.pixel_format = None }
          args
    | ("pixel_format", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.pixel_format = Some (to_string t) }
          args
    | ("hwaccel", { value = Ground (String "auto"); _ }) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.hwaccel = `Auto }
          args
    | ("hwaccel", { value = Ground (String "none"); _ }) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.hwaccel = `None }
          args
    | ("hwaccel_device", { value = Ground (String "none"); _ }) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.hwaccel_device = None }
          args
    | ("hwaccel_device", t) :: args ->
        parse_video_args ~opts
          { options with Ffmpeg_format.hwaccel_device = Some (to_string t) }
          args
    | arg :: args ->
        parse_opts opts arg;
        parse_video_args ~opts options args
  in

  let parse_stream ~content_type args =
    let opts = Hashtbl.create 0 in
    let codec = Option.map to_string (List.assoc_opt "codec" args) in
    match content_type with
      | `Audio ->
          let options = parse_audio_args ~opts default_audio args in
          (codec, `Audio options, opts)
      | `Video ->
          let options = parse_video_args ~opts default_video args in
          (codec, `Video options, opts)
  in

  List.fold_left
    (fun f -> function
      | `Encoder (field, `Copy None) ->
          {
            f with
            Ffmpeg_format.streams =
              f.Ffmpeg_format.streams @ [(field, `Copy `Wait_for_keyframe)];
          }
      | `Encoder (field, `Copy (Some t)) ->
          {
            f with
            Ffmpeg_format.streams =
              f.Ffmpeg_format.streams @ [(field, `Copy (to_copy_opt t))];
          }
      | `Encoder (field, `Encode (mode, content_type, args)) ->
          let codec, options, opts = parse_stream ~content_type args in
          {
            f with
            Ffmpeg_format.streams =
              f.Ffmpeg_format.streams
              @ [(field, `Encode { Ffmpeg_format.mode; options; codec; opts })];
          }
      | `Option ("format", { value = Ground (String "none"); _ }) ->
          { f with Ffmpeg_format.format = None }
      | `Option ("format", { value = Ground (String fmt); _ }) ->
          { f with Ffmpeg_format.format = Some fmt }
      | `Option (k, { value = Ground (String s); _ }) ->
          Hashtbl.add f.Ffmpeg_format.opts k (`String s);
          f
      | `Option (k, { value = Ground (Int i); _ }) ->
          Hashtbl.add f.Ffmpeg_format.opts k (`Int i);
          f
      | `Option (k, { value = Ground (Float i); _ }) ->
          Hashtbl.add f.Ffmpeg_format.opts k (`Float i);
          f
      | `Option (l, v) -> Lang_encoder.raise_generic_error (l, `Value v))
    defaults params

let make params =
  let params =
    List.map
      (function
        | _, `Encoder (name, args) ->
            let args =
              List.filter_map
                (function l, `Value v -> Some (l, v) | _, `Encoder _ -> None)
                args
            in
            `Encoder
              (parse_encoder_params ~to_pos:(fun t -> t.pos) (name, args))
        | l, `Value v -> `Option (l, v))
      params
  in
  Encoder.Ffmpeg (ffmpeg_gen params)

let () = Lang_encoder.register "ffmpeg" type_of_encoder make
