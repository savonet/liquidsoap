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

module ToAudioFrame =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

module FromAudioFrame =
  Swresample.Make (Swresample.Frame) (Swresample.FltPlanarBigArray)

module ToVideoFrame = Swscale.Make (Swscale.BigArray) (Swscale.Frame)
module FromVideoFrame = Swscale.Make (Swscale.Frame) (Swscale.BigArray)
module Generator = Generator.From_audio_video

(** From the script perspective, the operator sending data to a filter graph
  * is an output. *)
class audio_output ~name ~kind val_source =
  let noop () = () in
  object (self)
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~content_kind:kind ~name
          ~output_kind:"ffmpeg.filter.input" val_source true

    val mutable input = fun _ -> assert false

    method set_input fn = input <- fn

    val mutable pts = Int64.zero

    val mutable channels = None

    val mutable converter = None

    method private convert pcm =
      let in_channels = Array.length pcm in
      let mk_converter () =
        channels <- Some in_channels;
        let channels_layout =
          try Avutil.Channel_layout.get_default in_channels
          with Not_found ->
            failwith
              "ffmpeg filter: could not find a default channel configuration \
               for this number of channels.."
        in
        let samplerate = Frame.audio_of_seconds 1. in
        let c =
          ToAudioFrame.create ~out_sample_format:`Dbl channels_layout samplerate
            channels_layout samplerate
        in
        converter <- Some c;
        c
      in
      let converter =
        match (converter, channels) with
          | None, _ -> mk_converter ()
          | _, Some c when c <> in_channels ->
              self#log#important "Channels change detected!";
              mk_converter ()
          | Some c, _ -> c
      in
      ToAudioFrame.convert converter pcm

    method output_start = ()

    method output_stop = ()

    method output_reset = ()

    method output_send memo =
      let pcm = AFrame.content memo in
      let aframe = self#convert pcm in
      Avutil.frame_set_pts aframe pts;
      pts <- Int64.add (Int64.of_int (AFrame.position memo)) pts;
      input aframe
  end

class video_output ~name val_source =
  let content_kind = Frame.{ audio = Zero; video = Succ Zero; midi = Zero } in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let converter =
    ToVideoFrame.create [] width height `Yuv420p width height `Yuv420p
  in
  let noop () = () in
  object
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~content_kind ~name
          ~output_kind:"ffmpeg.filter.input" val_source true

    val mutable input : Swscale.Frame.t -> unit = fun _ -> assert false

    val mutable pts = Int64.zero

    method set_input fn = input <- fn

    method output_start = ()

    method output_stop = ()

    method output_reset = ()

    method output_send memo =
      let vbuf = VFrame.content memo in
      let vlen = VFrame.position memo in
      let vbuf = vbuf.(0) in
      for i = 0 to 0 + vlen - 1 do
        let f = Video.get vbuf i in
        let y, u, v = Image.YUV420.data f in
        let sy = Image.YUV420.y_stride f in
        let s = Image.YUV420.uv_stride f in
        let vdata = [| (y, sy); (u, s); (v, s) |] in
        let vframe = ToVideoFrame.convert converter vdata in
        Avutil.frame_set_pts vframe pts;
        pts <- Int64.succ pts;
        input vframe
      done
  end

type audio_config = {
  format : Avutil.Sample_format.t;
  rate : int;
  channels : int;
}

(* Same thing here. *)
class audio_input kind =
  let channels_layout channels =
    try Avutil.Channel_layout.get_default channels
    with Not_found ->
      failwith
        "ffmpeg filter: could not find a default channel configuration for \
         this number of channels.."
  in
  let out_samplerate = Frame.audio_of_seconds 1. in
  let generator = Generator.create `Audio in
  object (self)
    inherit Source.source kind ~name:"ffmpeg.filter.output"

    val mutable config = None

    val mutable converter = None

    method private convert frame =
      let in_config =
        {
          format = Avutil.Audio.frame_get_sample_format frame;
          rate = Avutil.Audio.frame_get_sample_rate frame;
          channels = Avutil.Audio.frame_get_channels frame;
        }
      in
      let out_config =
        {
          format = `Dbl;
          rate = out_samplerate;
          channels = Frame.((type_of_kind self#kind).audio);
        }
      in
      let mk_converter () =
        config <- Some (in_config, out_config);
        let c =
          FromAudioFrame.create ~in_sample_format:in_config.format
            ~out_sample_format:out_config.format
            (channels_layout in_config.channels)
            in_config.rate
            (channels_layout out_config.channels)
            out_config.rate
        in
        converter <- Some c;
        c
      in
      let converter =
        match (converter, config) with
          | None, _ -> mk_converter ()
          | _, Some c when c <> (in_config, out_config) ->
              self#log#important "Format change detected!";
              mk_converter ()
          | Some c, _ -> c
      in
      FromAudioFrame.convert converter frame

    val mutable output = fun _ -> assert false

    method set_output fn = output <- fn

    method self_sync = false

    method stype = Source.Fallible

    method remaining = Generator.remaining generator

    method private flush_buffer =
      let rec f () =
        try
          let pcm = self#convert (output ()) in
          Generator.put_audio generator pcm 0 (Audio.length pcm);
          f ()
        with Avutil.Error `Eagain -> ()
      in
      f ()

    method is_ready =
      self#flush_buffer;
      Generator.length generator > 0

    method private get_frame frame =
      self#flush_buffer;
      Generator.fill generator frame;
      if Frame.is_partial frame && Generator.length generator = 0 then
        self#log#important "Buffer emptied..."

    method abort_track = ()
  end

type video_config = {
  width : int;
  height : int;
  pixel_format : Avutil.Pixel_format.t;
}

class video_input kind =
  let generator = Generator.create `Video in
  let target_width = Lazy.force Frame.video_width in
  let target_height = Lazy.force Frame.video_height in
  object (self)
    inherit Source.source kind ~name:"ffmpeg.filter.output"

    val mutable config = None

    val mutable converter = None

    method private convert frame =
      let in_config =
        {
          width = Avutil.Video.frame_get_width frame;
          height = Avutil.Video.frame_get_height frame;
          pixel_format = Avutil.Video.frame_get_pixel_format frame;
        }
      in
      let mk_converter () =
        config <- Some in_config;
        let c =
          FromVideoFrame.create [] in_config.width in_config.height
            in_config.pixel_format target_width target_height `Yuv420p
        in
        converter <- Some c;
        c
      in
      let converter =
        match (converter, config) with
          | None, _ -> mk_converter ()
          | _, Some c when c <> in_config ->
              self#log#important "Format change detected!";
              mk_converter ()
          | Some c, _ -> c
      in
      FromVideoFrame.convert converter frame

    val mutable output = fun _ -> assert false

    method set_output fn = output <- fn

    method self_sync = false

    method stype = Source.Fallible

    method remaining = Generator.remaining generator

    method private flush_buffer =
      let rec f () =
        try
          let img =
            match self#convert (output ()) with
              | [| (y, sy); (u, s); (v, _) |] ->
                  Image.YUV420.make target_width target_height y sy u v s
              | _ -> assert false
          in
          let content = Video.single img in
          Generator.put_video generator [| content |] 0 (Video.length content);
          f ()
        with Avutil.Error `Eagain -> ()
      in
      f ()

    method is_ready =
      self#flush_buffer;
      Generator.length generator > 0

    method private get_frame frame =
      self#flush_buffer;
      Generator.fill generator frame;
      if Frame.is_partial frame && Generator.length generator = 0 then
        self#log#important "Buffer emptied..."

    method abort_track = ()
  end
