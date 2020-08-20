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

(** Connect sources to FFmpeg filters. *)

module Generator = Generator.From_audio_video

let noop () = ()

(** From the script perspective, the operator sending data to a filter graph
  * is an output. *)
class audio_output ~name ~kind val_source =
  let convert_frame_pts =
    Ffmpeg_utils.(
      convert_time_base ~src:(liq_frame_time_base ())
        ~dst:(liq_master_ticks_time_base ()))
  in
  object
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~content_kind:kind ~name
          ~output_kind:"ffmpeg.filter.input" val_source true

    val mutable input = fun _ -> assert false

    method set_input fn = input <- fn

    val mutable init = lazy ()

    method set_init v = init <- v

    method output_start = Lazy.force init

    method output_stop = ()

    method output_reset = ()

    method output_send memo =
      let frames =
        Ffmpeg_raw_content.(
          (Audio.get_data Frame.(memo.content.audio)).VideoSpecs.data)
      in
      List.iter
        (fun (pos, aframe) ->
          let pts =
            Int64.add (convert_frame_pts (Frame.pts memo)) (Int64.of_int pos)
          in
          Avutil.frame_set_pts aframe (Some pts);
          input aframe)
        frames
  end

class video_output ~kind ~name val_source =
  let convert_frame_pts =
    Ffmpeg_utils.(
      convert_time_base ~src:(liq_frame_time_base ())
        ~dst:(liq_master_ticks_time_base ()))
  in
  object
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~content_kind:kind ~name
          ~output_kind:"ffmpeg.filter.input" val_source true

    val mutable input : Swscale.Frame.t -> unit = fun _ -> assert false

    method set_input fn = input <- fn

    val mutable init = lazy ()

    method set_init v = init <- v

    method output_start = Lazy.force init

    method output_stop = ()

    method output_reset = ()

    method output_send memo =
      let frames =
        Ffmpeg_raw_content.(
          (Video.get_data Frame.(memo.content.video)).VideoSpecs.data)
      in
      List.iter
        (fun (pos, vframe) ->
          let pts =
            Int64.add (convert_frame_pts (Frame.pts memo)) (Int64.of_int pos)
          in
          Avutil.frame_set_pts vframe (Some pts);
          input vframe)
        frames
  end

type audio_config = {
  format : Avutil.Sample_format.t;
  rate : int;
  channels : int;
}

(* Same thing here. *)
class audio_input ~bufferize kind =
  let generator = Generator.create `Audio in
  let min_buf = Frame.master_of_seconds bufferize in
  let format =
    match kind.Frame.audio with `Format f -> f | _ -> assert false
  in
  object (self)
    inherit Source.source kind ~name:"ffmpeg.filter.output"

    val mutable config = None

    val mutable output = None

    method set_output v =
      let output_format =
        {
          Ffmpeg_raw_content.AudioSpecs.channel_layout =
            Avfilter.(channel_layout v.context);
          sample_rate = Avfilter.(sample_rate v.context);
          sample_format = Avfilter.(sample_format v.context);
        }
      in
      Frame_content.merge format
        (Ffmpeg_raw_content.Audio.lift_params [output_format]);
      output <- Some v

    method self_sync = false

    method stype = Source.Fallible

    method remaining = Generator.remaining generator

    method private flush_buffer =
      let output = Option.get output in
      let src = Avfilter.(time_base output.context) in
      let dst = Ffmpeg_utils.liq_frame_time_base () in
      let get_duration frame =
        let samplerate = float (Avutil.Audio.frame_get_sample_rate frame) in
        let nb_samples = float (Avutil.Audio.frame_nb_samples frame) in
        Frame.master_of_seconds (nb_samples /. samplerate)
      in
      let rec f () =
        try
          let frame = output.Avfilter.handler () in
          let content =
            {
              Ffmpeg_content_base.params =
                Ffmpeg_raw_content.AudioSpecs.frame_param frame;
              data = [(0, frame)];
            }
          in
          let pts =
            Option.map
              (Ffmpeg_utils.convert_time_base ~src ~dst)
              (Avutil.frame_pts frame)
          in
          Generator.put_audio ?pts generator
            (Ffmpeg_raw_content.Audio.lift_data content)
            0 (get_duration frame);
          f ()
        with Avutil.Error `Eagain -> ()
      in
      f ()

    val mutable state : [ `Ready | `Not_ready ] = `Not_ready

    method is_ready =
      if output <> None then self#flush_buffer;
      match state with
        | `Not_ready ->
            if Generator.length generator >= min_buf then (
              state <- `Ready;
              true )
            else false
        | `Ready ->
            if Generator.length generator > 0 then true
            else (
              state <- `Not_ready;
              false )

    method private get_frame frame =
      self#flush_buffer;
      Generator.fill generator frame;
      if Frame.is_partial frame && Generator.length generator = 0 then
        self#log#important "Buffer emptied..."

    method abort_track = ()

    val mutable init = lazy ()

    method set_init v = init <- v

    method output_start = Lazy.force init
  end

type video_config = {
  width : int;
  height : int;
  pixel_format : Avutil.Pixel_format.t;
}

class video_input ~bufferize ~fps kind =
  let generator = Generator.create `Video in
  let min_buf = Frame.master_of_seconds bufferize in
  let format =
    match kind.Frame.video with `Format f -> f | _ -> assert false
  in
  let duration =
    lazy (Frame.master_of_seconds (1. /. float (Lazy.force fps)))
  in
  object (self)
    inherit Source.source kind ~name:"ffmpeg.filter.output"

    val mutable output = None

    method set_output v =
      let output_format =
        {
          Ffmpeg_raw_content.VideoSpecs.width = Avfilter.(width v.context);
          height = Avfilter.(height v.context);
          pixel_format = Some Avfilter.(pixel_format v.context);
        }
      in
      Frame_content.merge format
        (Ffmpeg_raw_content.Video.lift_params [output_format]);
      output <- Some v

    method self_sync = false

    method stype = Source.Fallible

    method remaining = Generator.remaining generator

    method private flush_buffer =
      let output = Option.get output in
      let src = Avfilter.(time_base output.context) in
      let dst = Ffmpeg_utils.liq_frame_time_base () in
      let rec f () =
        try
          let frame = output.Avfilter.handler () in
          let pts =
            Option.map
              (Ffmpeg_utils.convert_time_base ~src ~dst)
              (Avutil.frame_pts frame)
          in
          let params = Ffmpeg_raw_content.VideoSpecs.frame_param frame in
          let content =
            { Ffmpeg_raw_content.VideoSpecs.params; data = [(0, frame)] }
          in
          Generator.put_video ?pts generator
            (Ffmpeg_raw_content.Video.lift_data content)
            0 (Lazy.force duration);
          f ()
        with Avutil.Error `Eagain -> ()
      in
      f ()

    method is_ready =
      if output <> None then self#flush_buffer;
      Generator.length generator > min_buf

    method private get_frame frame =
      self#flush_buffer;
      Generator.fill generator frame;
      if Frame.is_partial frame && Generator.length generator = 0 then
        self#log#important "Buffer emptied..."

    method abort_track = ()
  end
