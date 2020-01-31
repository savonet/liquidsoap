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

(** FFMPEG encoder *)

module Resampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame)

let log = Ffmpeg_config.log

type audio_stream =
  (Avutil.output, Avutil.audio) Av.stream
  * (Swresample.FltPlanarBigArray.t, Swresample.Frame.t) Swresample.ctx

type video_stream =
  (Avutil.output, Avutil.video) Av.stream
  * (Swscale.BigArray.t, Swscale.Frame.t) Swscale.ctx

type handler = {
  output : Avutil.output Avutil.container;
  audio_stream : audio_stream option;
  channels : int;
  video_stream : video_stream option;
  vchans : int;
}

(* Convert ffmpeg-specific options. *)
let convert_options opts =
  let convert name fn =
    match Hashtbl.find_opt opts name with
      | None -> ()
      | Some v -> Hashtbl.replace opts name (fn v)
  in
  convert "sample_fmt" (function
    | `String fmt -> `Int Avutil.Sample_format.(get_id (find fmt))
    | _ -> assert false);
  convert "channel_layout" (function
    | `String layout -> `Int Avutil.Channel_layout.(get_id (find layout))
    | _ -> assert false)

let mk_format ffmpeg =
  match (ffmpeg.Ffmpeg_format.format, ffmpeg.Ffmpeg_format.output) with
    | Some short_name, _ -> Av.Format.guess_output_format ~short_name ()
    | None, `Url filename -> Av.Format.guess_output_format ~filename ()
    | _ -> None

let mk_encoder ~ffmpeg ~options output =
  let audio_codec =
    Utils.maybe Avcodec.Audio.find_encoder ffmpeg.Ffmpeg_format.audio_codec
  in
  let video_codec =
    Utils.maybe Avcodec.Video.find_encoder ffmpeg.Ffmpeg_format.video_codec
  in
  let src_freq = Frame.audio_of_seconds 1. in
  let channels = ffmpeg.Ffmpeg_format.channels in
  if channels > 0 && audio_codec = None then
    failwith "Audio codec required when channels > 0";
  let vchans = if video_codec = None then 0 else 1 in
  let dst_freq = Lazy.force ffmpeg.Ffmpeg_format.samplerate in
  let video_width = Lazy.force Frame.video_width in
  let video_height = Lazy.force Frame.video_height in
  let audio_stream =
    Utils.maybe
      (fun audio_codec ->
        let opts =
          Av.mk_audio_opts ~channels
            ~sample_rate:(Lazy.force ffmpeg.Ffmpeg_format.samplerate)
            ()
        in
        let audio_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.audio_opts in
        Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.audio_opts;
        Hashtbl.iter (Hashtbl.add opts) options;
        let out_sample_format =
          Avcodec.Audio.find_best_sample_format audio_codec `Dbl
        in
        let channels_layout =
          try Avutil.Channel_layout.get_default ffmpeg.Ffmpeg_format.channels
          with Not_found ->
            failwith
              "%ffmpeg encoder: could not find a default channel configuration \
               for this number of channels.."
        in
        let resampler =
          Resampler.create ~out_sample_format channels_layout src_freq
            channels_layout dst_freq
        in
        let stream = Av.new_audio_stream ~opts ~codec:audio_codec output in
        Hashtbl.filter_map_inplace
          (fun l v -> if Hashtbl.mem opts l then Some v else None)
          audio_opts;
        if Hashtbl.length audio_opts > 0 then
          failwith
            (Printf.sprintf "Unrecognized options: %s"
               (Ffmpeg_format.string_of_options audio_opts));
        Hashtbl.filter_map_inplace
          (fun l v -> if Hashtbl.mem opts l then Some v else None)
          options;
        (stream, resampler))
      audio_codec
  in
  let video_stream =
    Utils.maybe
      (fun video_codec ->
        let pixel_format =
          Avcodec.Video.find_best_pixel_format video_codec `Yuv420p
        in
        let frame_rate = Lazy.force Frame.video_rate in
        let time_base = { Avutil.num = 1; den = frame_rate } in
        let scaler =
          Scaler.create [] video_width video_height `Yuv420p video_width
            video_height pixel_format
        in
        let opts =
          Av.mk_video_opts ~pixel_format ~frame_rate ~time_base
            ~size:(video_width, video_height)
            ()
        in
        let video_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.video_opts in
        Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.video_opts;
        Hashtbl.iter (Hashtbl.add opts) options;
        let stream = Av.new_video_stream ~opts ~codec:video_codec output in
        Hashtbl.filter_map_inplace
          (fun l v -> if Hashtbl.mem opts l then Some v else None)
          video_opts;
        if Hashtbl.length video_opts > 0 then
          failwith
            (Printf.sprintf "Unrecognized options: %s"
               (Ffmpeg_format.string_of_options video_opts));
        Hashtbl.filter_map_inplace
          (fun l v -> if Hashtbl.mem opts l then Some v else None)
          options;
        (stream, scaler))
      video_codec
  in
  { output; audio_stream; channels; video_stream; vchans }

let encode ~encoder frame start len =
  let content =
    Frame.content_of_type frame start
      { Frame.audio = encoder.channels; video = encoder.vchans; midi = 0 }
  in
  ignore
    (Utils.maybe
       (fun (stream, converter) ->
         let pcm = content.Frame.audio in
         let aframe = Resampler.convert converter pcm in
         Av.write_frame stream aframe)
       encoder.audio_stream);
  ignore
    (Utils.maybe
       (fun (stream, scaler) ->
         let vstart = Frame.video_of_master start in
         let vlen = Frame.video_of_master len in
         let vbuf = content.Frame.video in
         let vbuf = vbuf.(0) in
         for i = vstart to vstart + vlen - 1 do
           let f = Video.get vbuf i in
           let y, u, v = Image.YUV420.data f in
           let sy = Image.YUV420.y_stride f in
           let s = Image.YUV420.uv_stride f in
           let vdata = [| (y, sy); (u, s); (v, s) |] in
           let vframe = Scaler.convert scaler vdata in
           Av.write_frame stream vframe
         done)
       encoder.video_stream)

let insert_metadata ~encoder m =
  let m =
    Hashtbl.fold (fun lbl v l -> (lbl, v) :: l) (Meta_format.to_metadata m) []
  in
  match (encoder.audio_stream, encoder.video_stream) with
    | Some (s, _), _ -> Av.set_metadata s m
    | None, Some (s, _) -> Av.set_metadata s m
    | _ -> ()

let encoder ffmpeg meta =
  let buf = Strings.Mutable.empty () in
  let make () =
    let options = Hashtbl.copy ffmpeg.Ffmpeg_format.other_opts in
    convert_options options;
    let write str ofs len =
      Strings.Mutable.add_subbytes buf str ofs len;
      len
    in
    let format = mk_format ffmpeg in
    let output =
      match ffmpeg.Ffmpeg_format.output with
        | `Stream ->
            if format = None then failwith "format is required!";
            Av.open_output_stream ~opts:options write (Utils.get_some format)
        | `Url url -> Av.open_output ?format ~opts:options url
    in
    let ret = mk_encoder ~ffmpeg ~options output in
    if Hashtbl.length options > 0 then
      failwith
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options options));
    ret
  in
  let encoder = ref (make ()) in
  let encode frame start len =
    encode ~encoder:!encoder frame start len;
    Strings.Mutable.flush buf
  in
  let insert_metadata m =
    Av.close !encoder.output;
    encoder := make ();
    insert_metadata ~encoder:!encoder m
  in
  insert_metadata meta;
  let stop () =
    Av.close !encoder.output;
    Strings.Mutable.flush buf
  in
  { Encoder.insert_metadata; header = Strings.empty; encode; stop }

let () =
  Encoder.plug#register "FFMPEG" (function
    | Encoder.Ffmpeg m -> Some (fun _ -> encoder m)
    | _ -> None)
