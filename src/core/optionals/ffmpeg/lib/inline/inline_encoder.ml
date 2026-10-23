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

let per_unit n = { Avutil.num = 1; den = n }

let rescale ~src ~dst v =
  Option.value ~default:0 (Time_base.rescale ~src ~dst v)

let item_fault faults ~kind f =
  try f ()
  with Avutil.Error e -> (
    match Faults.item faults ~kind (Avutil.string_of_error e) with
      | `Item_fault -> ()
      | `Source_fault ->
          raise (Producer.Source_fault "too many consecutive conversion faults"))

let output_format (output : Producer.output) =
  Frame.Fields.find output.field
    (Generator.content_type output.buffer.generator)

(* inline-codecs.md §4.2: a build whose parameters fail the merge emits
   nothing for its source. *)
let merge_params (output : Producer.output) params =
  try Content.merge (output_format output) params
  with exn ->
    raise
      (Producer.Source_fault
         (Printf.sprintf "Encoder parameters do not match the output format: %s"
            (Printexc.to_string exn)))

let copy_data ~params ~source_id ~time_base wrap ~length packets =
  Ffmpeg_content.Copy.lift_data
    (Ffmpeg_content.Copy.Data.of_run (Some params)
       (Ffmpeg_content.Copy.Data.run ~source_params:params ~source_id ~time_base
          ~length
          (List.map (fun p -> (0, wrap p)) packets)))

let codec_name (encoded : F.encoded) =
  match encoded.codec with
    | Some name -> name
    | None -> failwith "Inline encoders need a codec"

let requested_sample_format (a : F.audio) =
  match a.sample_format with
    | Some name -> Avutil.Sample_format.find name
    | None -> `Dbl

(* The record and its counters: every transform of a record shares its
   timeline (inline-codecs.md §9). *)
let join input ~unit =
  let record = Source_ids.join input in
  let base = Source_ids.elapsed record in
  (record, base, rescale ~src:(Time_base.main ()) ~dst:unit base)

let ends record =
  let leave () = Source_ids.leave record in
  ( (fun () ->
      Source_ids.close record;
      leave ()),
    leave )

let audio ~(log : Log.t) ~(encoded : F.encoded) ~(a : F.audio) :
    Producer.builder =
 fun output ~input ->
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field output.field in
  let rate = Lazy.Mutexed.force a.samplerate in
  let target_tb = per_unit rate in
  let record, base, start = join input ~unit:target_tb in
  let source_id = Source_ids.id record in
  let layout = Avutil.Channel_layout.get_default a.channels in
  let input_layout =
    Avutil.Channel_layout.get_default
      (Content.Audio.channels_of_format
         (Frame.Fields.find output.field input#content_type))
  in
  let push_item, flush_items, frame_size, sample_format =
    match encoded.mode with
      | `Internal ->
          let codec = Avcodec.Audio.find_encoder_by_name (codec_name encoded) in
          let sample_format =
            Avcodec.Audio.find_best_sample_format codec
              (requested_sample_format a)
          in
          let encoder =
            Avcodec.Audio.create_encoder
              ~opts:(Opts.of_list encoded.opts)
              ~channel_layout:layout ~sample_rate:rate ~sample_format
              ~time_base:target_tb codec
          in
          let params = Copy_params.make (`Audio (Avcodec.params encoder)) in
          merge_params output (Ffmpeg_content.Copy.lift_params (Some params));
          let time_base = Avcodec.time_base encoder in
          let chunker =
            Chunker.packets ~faults ~src:time_base ~output
              ~to_data:
                (copy_data ~params ~source_id ~time_base (fun p -> `Audio p))
              ()
          in
          let frame_size =
            if List.mem `Variable_frame_size (Avcodec.capabilities codec) then
              None
            else (
              match Avcodec.Audio.frame_size encoder with
                | 0 -> None
                | n -> Some n)
          in
          ( (fun frame ->
              item_fault faults ~kind (fun () ->
                  Avcodec.encode encoder (Chunker.push chunker) frame)),
            (fun () ->
              Avcodec.flush_encoder encoder (Chunker.push chunker);
              Chunker.flush chunker),
            frame_size,
            sample_format )
      | `Raw ->
          let sample_format = requested_sample_format a in
          let params =
            {
              Raw_params.Audio.channel_layout = Some layout;
              sample_format = Some sample_format;
              sample_rate = Some rate;
            }
          in
          merge_params output (Ffmpeg_content.Audio.lift_params params);
          let chunker =
            Chunker.frames ~faults ~src:target_tb ~output
              ~to_data:(fun ~length frames ->
                Ffmpeg_content.Audio.lift_data
                  (Ffmpeg_content.Audio.Data.of_run params
                     (Ffmpeg_content.Audio.Data.run ~source_id
                        ~time_base:target_tb ~length
                        (List.map (fun f -> (0, f)) frames))))
              ()
          in
          ( Chunker.push chunker,
            (fun () -> Chunker.flush chunker),
            None,
            sample_format )
  in
  let module R =
    Swresample.Make (Swresample.PlanarFloatArray) (Swresample.Frame)
  in
  let resampler =
    R.create input_layout
      (Lazy.Mutexed.force Frame.audio_rate)
      layout ~out_sample_format:sample_format rate
  in
  let writer =
    Audio_writer.create ~frame_size
      ~params:{ sample_rate = rate; channel_layout = layout; sample_format }
      ~start push_item
  in
  let close, leave = ends record in
  {
    Producer.base;
    push =
      (fun ~input_time data ->
        let length = Content.length data in
        item_fault faults ~kind (fun () ->
            Audio_writer.feed writer
              (R.convert
                 ~length:(Frame.audio_of_main length)
                 resampler
                 (Content.Audio.get_data data)));
        Source_ids.advance record ~until:(input_time + length));
    drain =
      (fun () ->
        Fun.protect ~finally:close (fun () ->
            Audio_writer.feed writer (R.flush resampler);
            Audio_writer.flush writer;
            flush_items ()));
    discard = leave;
    close = (fun () -> Source_ids.close record);
  }

type video_stage = {
  fps : Helpers.Fps.t;
  encode : Avutil.video Avutil.frame -> unit;
  flush : unit -> unit;
}

let video ~(log : Log.t) ~(encoded : F.encoded) ~(v : F.video) :
    Producer.builder =
 fun output ~input ->
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field output.field in
  let input_rate = Lazy.Mutexed.force Frame.video_rate in
  let input_tb = per_unit input_rate in
  let record, base, first_image = join input ~unit:input_tb in
  let source_id = Source_ids.id record in
  let input_format = Frame.Fields.find output.field input#content_type in
  let input_width, input_height =
    Content.Video.dimensions_of_format input_format
  in
  let width = Lazy.Mutexed.force v.width in
  let height = Lazy.Mutexed.force v.height in
  let frame_rate = Lazy.Mutexed.force v.framerate in
  let codec =
    match encoded.mode with
      | `Internal ->
          Some (Avcodec.Video.find_encoder_by_name (codec_name encoded))
      | `Raw -> None
  in
  let target =
    match (codec, v.pixel_format) with
      | Some codec, configured ->
          let alpha =
            v.alpha
            || try Content.Video.alpha_of_format input_format with _ -> false
          in
          Pixel_formats.for_encoder ?configured ~alpha codec
      | None, Some name -> Avutil.Pixel_format.of_string name
      | None, None -> `Yuv420p
  in
  let alpha = Pixel_formats.has_alpha target in
  let module Scaler = Swscale.Make (Swscale.BigArray) (Swscale.Frame) in
  let scaler =
    Scaler.create
      ~threads:(max 0 Settings.scaling_threads#get)
      [Pixel_formats.scaling_flag ()]
      input_width input_height
      (if alpha then `Yuva420p else `Yuv420p)
      width height target
  in
  let stage_of (format : Avutil.Video.frame_format) =
    let fps =
      Helpers.Fps.create
        ~source_rate:{ Avutil.num = input_rate; den = 1 }
        ~target:frame_rate
        ~start:(`Pts (Int64.of_int first_image))
        ~time_base:input_tb
        { format with pixel_aspect = Some { Avutil.num = 1; den = 1 } }
    in
    let fps_tb = Helpers.Fps.time_base fps in
    match codec with
      | Some codec ->
          let opts = Opts.of_list encoded.opts in
          let hardware_context, pixel_format =
            Hw_context.select ~log ~hwaccel:v.hwaccel
              ?hardware_pixel_format:
                (Option.map Avutil.Pixel_format.of_string v.hwaccel_pixel_format)
              ?device:v.hwaccel_device ~opts ~target ~width ~height codec
          in
          let encoder =
            Avcodec.Video.create_encoder ~opts
              ~frame_rate:{ Avutil.num = frame_rate; den = 1 }
              ?hardware_context ~pixel_format ~width ~height ~time_base:fps_tb
              codec
          in
          let params =
            Copy_params.make
              ~frame_rate:{ Avutil.num = frame_rate; den = 1 }
              (`Video (Avcodec.params encoder))
          in
          merge_params output (Ffmpeg_content.Copy.lift_params (Some params));
          let time_base = Avcodec.time_base encoder in
          let chunker =
            Chunker.packets ~faults ~src:time_base ~output
              ~to_data:
                (copy_data ~params ~source_id ~time_base (fun p -> `Video p))
              ()
          in
          {
            fps;
            encode =
              (fun frame ->
                item_fault faults ~kind (fun () ->
                    Avcodec.encode encoder (Chunker.push chunker) frame));
            flush =
              (fun () ->
                Avcodec.flush_encoder encoder (Chunker.push chunker);
                Chunker.flush chunker);
          }
      | None ->
          let params =
            {
              Raw_params.Video.width = Some width;
              height = Some height;
              pixel_format = Some target;
              pixel_aspect = Some { Avutil.num = 1; den = 1 };
            }
          in
          merge_params output (Ffmpeg_content.Video.lift_params params);
          let chunker =
            Chunker.frames ~faults ~src:fps_tb ~output
              ~to_data:(fun ~length frames ->
                Ffmpeg_content.Video.lift_data
                  (Ffmpeg_content.Video.Data.of_run params
                     (Ffmpeg_content.Video.Data.run ~source_id ~time_base:fps_tb
                        ~length
                        (List.map (fun f -> (0, f)) frames))))
              ()
          in
          {
            fps;
            encode = Chunker.push chunker;
            flush = (fun () -> Chunker.flush chunker);
          }
  in
  let stage = ref None in
  let image_count = ref first_image in
  let push_image image =
    let image =
      Mm.Video.Canvas.Image.render ~transparent:alpha
        (Mm.Video.Canvas.Image.viewport input_width input_height image)
    in
    if alpha then Mm.Image.YUV420.ensure_alpha image;
    let frame = Scaler.convert scaler (Image_pack.pack image) in
    Avutil.Frame.set_pts frame (Some (Int64.of_int !image_count));
    Avutil.Frame.set_duration frame (Some 1L);
    incr image_count;
    let s =
      match !stage with
        | Some s -> s
        | None ->
            let s = stage_of (Avutil.Video.frame_format frame) in
            stage := Some s;
            s
    in
    Helpers.Fps.convert s.fps s.encode frame
  in
  let close, leave = ends record in
  {
    Producer.base;
    push =
      (fun ~input_time data ->
        List.iter
          (fun (_, image) ->
            item_fault faults ~kind (fun () -> push_image image))
          (Content.Video.get_data data).data;
        Source_ids.advance record ~until:(input_time + Content.length data));
    drain =
      (fun () ->
        Fun.protect ~finally:close (fun () ->
            Option.iter
              (fun s ->
                Helpers.Fps.eof s.fps s.encode;
                s.flush ())
              !stage));
    discard = leave;
    close = (fun () -> Source_ids.close record);
  }
