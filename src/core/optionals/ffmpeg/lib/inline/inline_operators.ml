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

let track_ffmpeg = Lang_modules.track_ffmpeg
let track_ffmpeg_raw = Lang_modules.track_ffmpeg_raw
let encode_module = Lang.add_module ~base:track_ffmpeg "encode"
let raw_encode_module = Lang.add_module ~base:track_ffmpeg_raw "encode"
let decode_module = Lang.add_module ~base:track_ffmpeg "decode"
let raw_decode_module = Lang.add_module ~base:track_ffmpeg_raw "decode"
let ffmpeg_module = Lang_modules.ffmpeg
let ffmpeg_raw = Lang.add_module ~base:ffmpeg_module "raw"

let () =
  List.iter
    (fun base ->
      ignore (Lang.add_module ~base "encode");
      ignore (Lang.add_module ~base "decode"))
    [ffmpeg_module; ffmpeg_raw]

let kind_t kind = Type.make (Format_type.descr (`Kind kind))
let media_name = function `Audio -> "audio" | `Video -> "video"
let invalid value message = raise (Error.Invalid_value (value, message, []))

(* inline-codecs.md §4, evaluation checks; the stream is looked up under
   [audio] or [video] whatever the input's field. *)
let stream_of_format ~media ~mode value : F.encoded =
  let format =
    match Lang_encoder.L.to_format value with
      | Encoder.Ffmpeg format -> format
      | _ -> invalid value "Inline encoders need a %ffmpeg format"
  in
  if format.F.opts <> [] then
    invalid value "Muxer options are not supported for inline encoders";
  if format.format <> None then
    invalid value "The format option is not supported for inline encoders";
  match List.assoc_opt (media_name media) format.streams with
    | Some (`Encode encoded) -> (
        match (mode, encoded) with
          | `Raw, { opts = _ :: _ as opts; _ } ->
              invalid value
                (Printf.sprintf "Unrecognized options: %s"
                   (String.concat ", " (List.map fst opts)))
          | `Internal, { codec = None; _ } ->
              invalid value "Inline encoders need a codec"
          | `Internal, { codec = Some name; options } ->
              let found =
                match options with
                  | `Audio _ -> (
                      try
                        ignore (Avcodec.Audio.find_encoder_by_name name);
                        true
                      with _ -> false)
                  | `Video _ -> (
                      try
                        ignore (Avcodec.Video.find_encoder_by_name name);
                        true
                      with _ -> false)
                  | `Subtitle _ -> false
              in
              if not found then
                invalid value (Printf.sprintf "Encoder not found: %s" name);
              encoded
          | `Raw, _ -> encoded)
    | _ ->
        invalid value
          (Printf.sprintf "The format has no %s stream" (media_name media))

(* inline-codecs.md §4, start-up check: the encoder opened once with the
   stream's full option set, then closed. *)
let startup_check value (encoded : F.encoded) () =
  match (encoded.mode, encoded.codec) with
    | `Raw, _ | _, None -> ()
    | `Internal, Some name -> (
        let opts = Opts.of_list encoded.opts in
        (try
           match encoded.options with
             | `Audio (a : F.audio) ->
                 let codec = Avcodec.Audio.find_encoder_by_name name in
                 let rate = Lazy.Mutexed.force a.samplerate in
                 ignore
                   (Avcodec.Audio.create_encoder ~opts
                      ~channel_layout:
                        (Avutil.Channel_layout.get_default a.channels)
                      ~sample_rate:rate
                      ~sample_format:
                        (Avcodec.Audio.find_best_sample_format codec
                           (match a.sample_format with
                             | Some s -> Avutil.Sample_format.find s
                             | None -> `Dbl))
                      ~time_base:{ Avutil.num = 1; den = rate }
                      codec)
             | `Video (v : F.video) ->
                 let codec = Avcodec.Video.find_encoder_by_name name in
                 let rate = Lazy.Mutexed.force v.framerate in
                 ignore
                   (Avcodec.Video.create_encoder ~opts
                      ~frame_rate:{ Avutil.num = rate; den = 1 }
                      ~pixel_format:
                        (Pixel_formats.for_encoder ?configured:v.pixel_format
                           ~alpha:v.alpha codec)
                      ~width:(Lazy.Mutexed.force v.width)
                      ~height:(Lazy.Mutexed.force v.height)
                      ~time_base:{ Avutil.num = 1; den = rate }
                      codec)
             | `Subtitle _ -> ()
         with Avutil.Error e -> invalid value (Avutil.string_of_error e));
        match Opts.unused_message [("stream", opts)] with
          | Some message -> invalid value message
          | None -> ())

let infallible_note =
  " The input is read in a child clock: any other output reading the same \
   input runs at the pace this operator reads it; put a `buffer` between them \
   to decouple. With an infallible input, a persistent data fault is a hard \
   error: use `mksafe` or `fallback` to keep streaming through it."

let register ~base ~name ~descr ~input_t ~return_t ?format_t ~builder () =
  let proto =
    (match format_t with
      | Some t -> [("", t, None, Some "Encoding format.")]
      | None -> [])
    @ [("", input_t, None, None)]
  in
  ignore
    (Lang.add_track_operator ~base name proto ~return_t ~category:`Conversion
       ~descr:(descr ^ infallible_note) (fun p ->
         let id =
           Lang.to_default_option ~default:name Lang.to_string
             (List.assoc "id" p)
         in
         let input =
           Lang.assoc "" (match format_t with Some _ -> 2 | None -> 1) p
         in
         let field, source = Lang.to_track input in
         let check, builder = builder p in
         ( field,
           Producer.create ~name:(id ^ ".producer") ~field ~input_t ~check
             ~builder (Lang.source source) )))

let format_t field_t media =
  Lang.format_t
    (Lang.frame_t (Lang.univ_t ())
       (Frame.Fields.add
          (Frame.Fields.register (media_name media))
          field_t Frame.Fields.empty))

let encoder ~media ~mode =
  let input_t, field_t, return_t =
    match (media, mode) with
      | `Audio, `Internal ->
          let a = Format_type.audio () in
          (a, a, kind_t Ffmpeg_content.Copy.kind)
      | `Video, `Internal ->
          let v = Format_type.video () in
          (v, v, kind_t Ffmpeg_content.Copy.kind)
      | `Audio, `Raw ->
          let r = kind_t Ffmpeg_content.Audio.kind in
          (Format_type.audio (), r, r)
      | `Video, `Raw ->
          let r = kind_t Ffmpeg_content.Video.kind in
          (Format_type.video (), r, r)
  in
  let log = Log.make ["ffmpeg"; "inline"; "encoder"] in
  register
    ~base:
      (match mode with `Internal -> encode_module | `Raw -> raw_encode_module)
    ~name:(media_name media)
    ~descr:
      (match mode with
        | `Internal -> "Encode a track to `ffmpeg.copy` content with FFmpeg."
        | `Raw -> "Convert a track to raw FFmpeg frames.")
    ~input_t ~return_t ~format_t:(format_t field_t media)
    ~builder:(fun p ->
      let value = Lang.assoc "" 1 p in
      let encoded = stream_of_format ~media ~mode value in
      ( Some (startup_check value encoded),
        match encoded.options with
          | `Audio a -> Inline_encoder.audio ~log ~encoded ~a
          | `Video v -> Inline_encoder.video ~log ~encoded ~v
          | `Subtitle _ -> invalid value "Subtitles cannot be encoded inline" ))
    ()

let decoder ~media ~mode =
  let input_t =
    match mode with
      | `Copy -> kind_t Ffmpeg_content.Copy.kind
      | `Raw -> (
          match media with
            | `Audio -> kind_t Ffmpeg_content.Audio.kind
            | `Video -> kind_t Ffmpeg_content.Video.kind)
  in
  let return_t =
    match media with
      | `Audio -> Format_type.audio ()
      | `Video -> Format_type.video ()
  in
  let log = Log.make ["ffmpeg"; "inline"; "decoder"] in
  register
    ~base:(match mode with `Copy -> decode_module | `Raw -> raw_decode_module)
    ~name:(media_name media)
    ~descr:
      (match mode with
        | `Copy -> "Decode `ffmpeg.copy` content with FFmpeg."
        | `Raw -> "Convert raw FFmpeg frames to internal content.")
    ~input_t ~return_t
    ~builder:(fun _ ->
      ( None,
        match media with
          | `Audio -> Inline_decoder.audio ~log ~mode
          | `Video -> Inline_decoder.video ~log ~mode ))
    ()

let () =
  List.iter
    (fun media ->
      encoder ~media ~mode:`Internal;
      encoder ~media ~mode:`Raw;
      decoder ~media ~mode:`Copy;
      decoder ~media ~mode:`Raw)
    [`Audio; `Video]
