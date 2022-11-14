(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let noop () = ()

(** From the script perspective, the operator sending data to a filter graph
  * is an output. *)
class audio_output ~pass_metadata ~name ~frame_t source_val =
  let convert_frame_pts =
    lazy
      Ffmpeg_utils.(
        convert_time_base ~src:(liq_frame_time_base ())
          ~dst:(liq_main_ticks_time_base ()))
  in
  object (self)
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~name
          ~output_kind:"ffmpeg.filter.input" source_val true

    inherit Source.no_seek

    initializer
    Typing.(
      self#frame_type <: frame_t;
      (Lang.to_source source_val)#frame_type <: self#frame_type)

    val mutable input = fun _ -> ()
    method set_input fn = input <- fn
    val mutable init : Avutil.audio Avutil.frame -> unit = fun _ -> assert false
    method set_init v = init <- v
    method start = ()
    method stop = ()
    method reset = ()

    method send_frame memo =
      let frames =
        Ffmpeg_raw_content.(
          (Audio.get_data (AFrame.content memo)).AudioSpecs.data)
      in
      List.iter
        (fun (pos, { Ffmpeg_raw_content.frame }) ->
          init frame;
          let pts =
            Int64.add
              ((Lazy.force convert_frame_pts) self#nb_frames)
              (Int64.of_int pos)
          in
          Avutil.Frame.set_pts frame (Some pts);
          if pass_metadata then (
            (* Pass only one metadata. *)
            match Frame.get_all_metadata memo with
              | (_, m) :: _ ->
                  let m = Hashtbl.fold (fun k v m -> (k, v) :: m) m [] in
                  Avutil.Frame.set_metadata frame m
              | _ -> ());
          input frame)
        frames
  end

class video_output ~pass_metadata ~name ~frame_t source_val =
  let convert_frame_pts =
    lazy
      Ffmpeg_utils.(
        convert_time_base ~src:(liq_frame_time_base ())
          ~dst:(liq_main_ticks_time_base ()))
  in
  object (self)
    inherit
      Output.output
        ~infallible:false ~on_stop:noop ~on_start:noop ~name
          ~output_kind:"ffmpeg.filter.input" source_val true

    initializer
    Typing.(
      self#frame_type <: frame_t;
      (Lang.to_source source_val)#frame_type <: self#frame_type)

    val mutable input : Swscale.Frame.t -> unit = fun _ -> ()
    method set_input fn = input <- fn
    val mutable init : Avutil.video Avutil.frame -> unit = fun _ -> assert false
    method set_init v = init <- v
    method start = ()
    method stop = ()
    method reset = ()

    method send_frame memo =
      let frames =
        Ffmpeg_raw_content.(
          (Video.get_data (VFrame.content memo)).VideoSpecs.data)
      in
      List.iter
        (fun (pos, { Ffmpeg_raw_content.frame }) ->
          init frame;
          let pts =
            Int64.add
              ((Lazy.force convert_frame_pts) self#nb_frames)
              (Int64.of_int pos)
          in
          Avutil.Frame.set_pts frame (Some pts);
          if pass_metadata then (
            (* Pass only one metadata. *)
            match Frame.get_all_metadata memo with
              | (_, m) :: _ ->
                  let m = Hashtbl.fold (fun k v m -> (k, v) :: m) m [] in
                  Avutil.Frame.set_metadata frame m
              | _ -> ());
          input frame)
        frames
  end

class virtual ['a] input_base ~self_sync_type ~self_sync ~is_ready ~pull =
  object (self)
    method virtual flush_buffer : 'a Avfilter.output -> unit -> unit
    method virtual buffer : Generator.t
    val mutable output = None

    method self_sync : Source.self_sync =
      (Lazy.force self_sync_type, self_sync ())

    method pull =
      pull ();
      ignore
        (Option.map
           (fun output ->
             let flush = self#flush_buffer output in
             let rec f () =
               try
                 while Generator.length self#buffer < Lazy.force Frame.size do
                   flush ()
                 done
               with Avutil.Error `Eagain ->
                 if is_ready () then (
                   pull ();
                   f ())
             in
             f ())
           output)

    method is_ready =
      Generator.length self#buffer >= Lazy.force Frame.size || is_ready ()

    method private get_frame frame =
      let b = Frame.breaks frame in
      if Generator.length self#buffer < Lazy.force Frame.size then self#pull;
      Generator.fill self#buffer frame;
      if List.length b + 1 <> List.length (Frame.breaks frame) then (
        let cur_pos = Frame.position frame in
        Frame.set_breaks frame (b @ [cur_pos]))
  end

type audio_config = {
  format : Avutil.Sample_format.t;
  rate : int;
  channels : int;
}

(* Same thing here. *)
class audio_input ~self_sync_type ~self_sync ~is_ready ~pull ~pass_metadata
  frame_t =
  let stream_idx = Ffmpeg_content_base.new_stream_idx () in
  object (self)
    inherit Source.source ~name:"ffmpeg.filter.output" ()
    inherit Source.no_seek
    inherit [[ `Audio ]] input_base ~self_sync_type ~self_sync ~is_ready ~pull
    initializer Typing.(self#frame_type <: frame_t)
    val mutable config = None

    method set_output v =
      let output_format =
        {
          Ffmpeg_raw_content.AudioSpecs.channel_layout =
            Some Avfilter.(channel_layout v.context);
          sample_rate = Some Avfilter.(sample_rate v.context);
          sample_format = Some Avfilter.(sample_format v.context);
        }
      in
      Content.merge
        (Option.get
           (Frame.Fields.find_opt Frame.Fields.audio self#content_type))
        (Ffmpeg_raw_content.Audio.lift_params output_format);
      output <- Some v

    method stype = `Fallible
    method remaining = Generator.remaining self#buffer

    method private flush_buffer output =
      let ffmpeg_frame_time_base = Avfilter.(time_base output.context) in
      let get_duration frame =
        let samplerate = float (Avutil.Audio.frame_get_sample_rate frame) in
        let nb_samples = float (Avutil.Audio.frame_nb_samples frame) in
        Frame.main_of_seconds (nb_samples /. samplerate)
      in
      fun () ->
        let ffmpeg_frame = output.Avfilter.handler () in
        if pass_metadata then (
          let metadata = Avutil.Frame.metadata ffmpeg_frame in
          if metadata <> [] then (
            let m = Hashtbl.create (List.length metadata) in
            List.iter (fun (k, v) -> Hashtbl.add m k v) metadata;
            Generator.add_metadata self#buffer m));
        let frame =
          {
            Ffmpeg_raw_content.time_base = ffmpeg_frame_time_base;
            frame = ffmpeg_frame;
            stream_idx;
          }
        in
        let length = get_duration ffmpeg_frame in
        let content =
          {
            Ffmpeg_content_base.params =
              Ffmpeg_raw_content.AudioSpecs.frame_params frame;
            data = [(0, frame)];
            length;
          }
        in
        Generator.put self#buffer Frame.Fields.audio
          (Ffmpeg_raw_content.Audio.lift_data content)

    method abort_track = ()
  end

type video_config = {
  width : int;
  height : int;
  pixel_format : Avutil.Pixel_format.t;
}

class video_input ~self_sync_type ~self_sync ~is_ready ~pull ~pass_metadata ~fps
  frame_t =
  let duration = lazy (Frame.main_of_seconds (1. /. float (Lazy.force fps))) in
  let stream_idx = Ffmpeg_content_base.new_stream_idx () in
  object (self)
    inherit Source.source ~name:"ffmpeg.filter.output" ()
    inherit Source.no_seek
    inherit [[ `Video ]] input_base ~self_sync_type ~self_sync ~is_ready ~pull
    initializer Typing.(self#frame_type <: frame_t)

    method set_output v =
      let output_format =
        {
          Ffmpeg_raw_content.VideoSpecs.width = Some Avfilter.(width v.context);
          height = Some Avfilter.(height v.context);
          pixel_format = Some Avfilter.(pixel_format v.context);
          pixel_aspect = Avfilter.(pixel_aspect v.context);
        }
      in
      Content.merge
        (Option.get
           (Frame.Fields.find_opt Frame.Fields.video self#content_type))
        (Ffmpeg_raw_content.Video.lift_params output_format);
      output <- Some v

    method stype = `Fallible
    method remaining = Generator.remaining self#buffer

    method private flush_buffer output =
      let ffmpeg_frame_time_base = Avfilter.(time_base output.context) in
      fun () ->
        let ffmpeg_frame = output.Avfilter.handler () in
        if pass_metadata then (
          let metadata = Avutil.Frame.metadata ffmpeg_frame in
          if metadata <> [] then (
            let m = Hashtbl.create (List.length metadata) in
            List.iter (fun (k, v) -> Hashtbl.add m k v) metadata;
            Generator.add_metadata self#buffer m));
        let frame =
          {
            Ffmpeg_raw_content.time_base = ffmpeg_frame_time_base;
            frame = ffmpeg_frame;
            stream_idx;
          }
        in
        let params = Ffmpeg_raw_content.VideoSpecs.frame_params frame in
        let length = Lazy.force duration in
        let content =
          { Ffmpeg_raw_content.VideoSpecs.params; data = [(0, frame)]; length }
        in
        Generator.put self#buffer Frame.Fields.video
          (Ffmpeg_raw_content.Video.lift_data content)

    method abort_track = ()
  end
