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

let log = Ffmpeg_utils.log

type audio_stream =
  (Avutil.output, Avutil.audio) Av.stream * (Frame.t -> int -> int -> unit)

type video_stream =
  (Avutil.output, Avutil.video) Av.stream * (Frame.t -> int -> int -> unit)

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
    | short_name, `Url filename ->
        Av.Format.guess_output_format ~filename ?short_name ()
    | Some short_name, _ -> Av.Format.guess_output_format ~short_name ()
    | _ -> None

let get_channel_layout channels =
  try Avutil.Channel_layout.get_default channels
  with Not_found ->
    failwith
      (Printf.sprintf
         "%%ffmpeg encoder: could not find a default channel configuration for \
          %d channels.."
         channels)

let mk_encoder ~ffmpeg ~options output =
  let audio_codec =
    Utils.maybe Avcodec.Audio.find_encoder ffmpeg.Ffmpeg_format.audio_codec
  in

  let channels =
    match audio_codec with Some _ -> ffmpeg.Ffmpeg_format.channels | None -> 0
  in

  let video_codec =
    Utils.maybe Avcodec.Video.find_encoder ffmpeg.Ffmpeg_format.video_codec
  in

  let vchans = match video_codec with Some _ -> 1 | None -> 0 in

  let audio_stream =
    Utils.maybe
      (fun audio_codec ->
        let src_samplerate = Lazy.force Frame.audio_rate in
        let src_liq_internal_audio_time_base =
          { Avutil.num = 1; den = src_samplerate }
        in
        let src_channels = ffmpeg.Ffmpeg_format.channels in
        let src_channel_layout = get_channel_layout src_channels in

        let target_samplerate = Lazy.force ffmpeg.Ffmpeg_format.samplerate in
        let target_liq_internal_audio_time_base =
          { Avutil.num = 1; den = target_samplerate }
        in
        let target_channels = ffmpeg.Ffmpeg_format.channels in
        let target_channel_layout = get_channel_layout target_channels in
        let target_sample_format =
          Avcodec.Audio.find_best_sample_format audio_codec `Dbl
        in

        let opts = Hashtbl.create 10 in
        Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.audio_opts;
        Hashtbl.iter (Hashtbl.add opts) options;

        let resampler =
          Resampler.create ~out_sample_format:target_sample_format
            src_channel_layout src_samplerate target_channel_layout
            target_samplerate
        in

        let stream =
          Av.new_audio_stream ~sample_rate:target_samplerate
            ~time_base:target_liq_internal_audio_time_base
            ~channel_layout:target_channel_layout
            ~sample_format:target_sample_format ~opts ~codec:audio_codec output
        in

        let audio_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.audio_opts in

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

        let write_frame =
          let variable_frame_size =
            List.mem `Variable_frame_size
              (Avcodec.Audio.capabilities audio_codec)
          in

          if variable_frame_size then Av.write_frame stream
          else (
            let out_frame_size = Av.get_frame_size stream in

            let frame_time_base =
              { Avutil.num = out_frame_size; den = target_samplerate }
            in

            let pts = ref 0L in
            let write_frame frame =
              let frame_pts =
                Ffmpeg_utils.convert_pts ~src:frame_time_base
                  ~dst:target_liq_internal_audio_time_base !pts
              in
              pts := Int64.succ !pts;
              Avutil.frame_set_pts frame (Some frame_pts);
              Av.write_frame stream frame
            in

            let in_params =
              {
                Avfilter.Utils.sample_rate = target_samplerate;
                channel_layout = target_channel_layout;
                sample_format = target_sample_format;
              }
            in
            let filter_in, filter_out =
              Avfilter.Utils.convert_audio ~in_params
                ~in_time_base:target_liq_internal_audio_time_base
                ~out_frame_size ()
            in
            let rec flush () =
              try
                write_frame (filter_out ());
                flush ()
              with Avutil.Error `Eagain -> ()
            in
            fun frame ->
              filter_in frame;
              flush () )
        in

        let cb frame start len =
          let astart = Frame.audio_of_master start in
          let alen = Frame.audio_of_master len in
          let pcm = Audio.sub (AFrame.content frame) astart alen in
          let aframe = Resampler.convert resampler pcm in
          let frame_pts =
            Ffmpeg_utils.convert_pts ~src:src_liq_internal_audio_time_base
              ~dst:target_liq_internal_audio_time_base (Frame.pts frame)
          in
          Avutil.frame_set_pts aframe (Some frame_pts);
          write_frame aframe
        in

        (stream, cb))
      audio_codec
  in
  let video_stream =
    Utils.maybe
      (fun video_codec ->
        let pixel_aspect = { Avutil.num = 1; den = 1 } in

        let src_fps = Lazy.force Frame.video_rate in
        let src_liq_internal_video_time_base =
          { Avutil.num = 1; den = src_fps }
        in
        let src_width = Lazy.force Frame.video_width in
        let src_height = Lazy.force Frame.video_height in

        let target_fps = Lazy.force ffmpeg.Ffmpeg_format.framerate in
        let target_liq_internal_video_time_base =
          { Avutil.num = 1; den = target_fps }
        in
        let target_width = Lazy.force ffmpeg.Ffmpeg_format.width in
        let target_height = Lazy.force ffmpeg.Ffmpeg_format.height in

        let flag =
          match Ffmpeg_utils.conf_scaling_algorithm#get with
            | "fast_bilinear" -> Swscale.Fast_bilinear
            | "bilinear" -> Swscale.Bilinear
            | "bicubic" -> Swscale.Bicubic
            | _ -> failwith "Invalid value set for ffmpeg scaling algorithm!"
        in
        let scaler =
          Scaler.create [flag] src_width src_height `Yuv420p target_width
            target_height `Yuv420p
        in

        let opts = Hashtbl.create 10 in
        Hashtbl.iter (Hashtbl.add opts) ffmpeg.Ffmpeg_format.video_opts;
        Hashtbl.iter (Hashtbl.add opts) options;

        let stream =
          Av.new_video_stream ~time_base:target_liq_internal_video_time_base
            ~pixel_format:`Yuv420p
            ~frame_rate:{ Avutil.num = target_fps; den = 1 }
            ~width:target_width ~height:target_height ~opts ~codec:video_codec
            output
        in

        let video_opts = Hashtbl.copy ffmpeg.Ffmpeg_format.video_opts in
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

        let target_pts = ref 0L in
        let converter =
          Ffmpeg_utils.Fps.init ~width:target_width ~height:target_height
            ~pixel_format:`Yuv420p ~time_base:src_liq_internal_video_time_base
            ~pixel_aspect ~source_fps:src_fps ~target_fps ()
        in
        let fps_converter frame =
          Ffmpeg_utils.Fps.convert converter frame (fun frame ->
              Avutil.frame_set_pts frame (Some !target_pts);
              target_pts := Int64.succ !target_pts;
              Av.write_frame stream frame)
        in
        let pts = ref 0L in
        let cb frame start len =
          let vstart = Frame.video_of_master start in
          let vlen = Frame.video_of_master len in
          let vbuf = VFrame.content frame in
          let vbuf = vbuf.(0) in
          for i = vstart to vstart + vlen - 1 do
            let f = Video.get vbuf i in
            let y, u, v = Image.YUV420.data f in
            let sy = Image.YUV420.y_stride f in
            let s = Image.YUV420.uv_stride f in
            let vdata = [| (y, sy); (u, s); (v, s) |] in
            let vframe = Scaler.convert scaler vdata in
            Avutil.frame_set_pts vframe (Some !pts);
            pts := Int64.succ !pts;
            fps_converter vframe
          done
        in
        (stream, cb))
      video_codec
  in
  { output; audio_stream; channels; video_stream; vchans }

let encode ~encoder frame start len =
  ignore (Utils.maybe (fun (_, cb) -> cb frame start len) encoder.audio_stream);
  ignore (Utils.maybe (fun (_, cb) -> cb frame start len) encoder.video_stream)

let insert_metadata ~encoder m =
  let m =
    Hashtbl.fold (fun lbl v l -> (lbl, v) :: l) (Meta_format.to_metadata m) []
  in
  if not (Av.output_started encoder.output) then
    Av.set_output_metadata encoder.output m

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
  let insert_metadata m = insert_metadata ~encoder:!encoder m in
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
