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

(** Decode and read metadata using ffmpeg. *)

type 'a sparse_decoder = {
  decoder : buffer:Decoder.buffer -> 'a -> unit;
  advance : buffer:Decoder.buffer -> int -> unit;
}

let log = Log.make ["decoder"; "ffmpeg"]

let conf_ffmpeg_decoder =
  Dtools.Conf.unit
    ~p:(Decoder.conf_decoder#plug "ffmpeg")
    "FFmpeg decoder configuration"

let conf_codecs =
  Dtools.Conf.unit ~p:(conf_ffmpeg_decoder#plug "codecs") "Codecs settings"

let conf_max_interleave_duration =
  Dtools.Conf.float
    ~p:(conf_ffmpeg_decoder#plug "max_interleave_duration")
    ~d:5. "Maximum data buffered while waiting for all streams."

let conf_codecs =
  let codecs = Hashtbl.create 10 in
  List.iter
    (fun c ->
      let id = Avcodec.Audio.get_id c in
      let codec_name = Avcodec.Audio.string_of_id id in
      let name = Avcodec.Audio.get_name c in
      let c =
        match Hashtbl.find_opt codecs codec_name with
          | Some l -> name :: l
          | None -> [name]
      in
      Hashtbl.replace codecs codec_name c)
    Avcodec.Audio.decoders;

  List.iter
    (fun c ->
      let id = Avcodec.Video.get_id c in
      let codec_name = Avcodec.Video.string_of_id id in
      let name = Avcodec.Video.get_name c in
      let c =
        match Hashtbl.find_opt codecs codec_name with
          | Some l -> name :: l
          | None -> [name]
      in
      Hashtbl.replace codecs codec_name c)
    Avcodec.Video.decoders;

  Hashtbl.fold
    (fun name codecs l ->
      let conf =
        Dtools.Conf.string ~p:(conf_codecs#plug name)
          ("Preferred codec to decode " ^ name)
      in
      ignore
        (Dtools.Conf.list ~p:(conf#plug "available") ~d:codecs
           ("Available codecs to decode " ^ name));
      (name, conf) :: l)
    codecs []

let configure_stream ~get_name ~find_decoder params =
  let name = get_name params in
  match List.assoc_opt name conf_codecs with
    | Some conf -> (
        try
          let preferred = conf#get in
          log#info "Trying preferred decoder %s for codec %s" preferred name;
          try { Av.codec = Some (find_decoder preferred); opts = None }
          with exn ->
            let bt = Printexc.get_backtrace () in
            Utils.log_exception ~log ~bt
              (Printf.sprintf "Failed to set decoder %s for codec %s: %s"
                 preferred name (Printexc.to_string exn));
            { Av.codec = None; opts = None }
        with _ -> { Av.codec = None; opts = None })
    | None -> { Av.codec = None; opts = None }

let configure_audio_stream params =
  configure_stream
    ~get_name:(fun p -> Avcodec.Audio.(string_of_id (get_params_id p)))
    ~find_decoder:Avcodec.Audio.find_decoder_by_name params

let configure_video_stream params =
  configure_stream
    ~get_name:(fun p -> Avcodec.Video.(string_of_id (get_params_id p)))
    ~find_decoder:Avcodec.Video.find_decoder_by_name params

let configure_subtitle_stream params =
  configure_stream
    ~get_name:(fun p -> Avcodec.Subtitle.(string_of_id (get_params_id p)))
    ~find_decoder:Avcodec.Subtitle.find_decoder_by_name params

let mk_subtitle_decoder ~output ~process () =
  let current_position = ref None in
  let advance ~buffer position =
    match !current_position with
      | None ->
          output ?data:None ~buffer ~length:position ();
          current_position := Some (position, None)
      | Some (p, _) when position <= p -> ()
      | Some (p, data) ->
          let data = Option.map snd data in
          output ?data ~buffer ~length:(position - p) ();
          current_position := Some (position, None)
  in
  let process ~buffer subtitle =
    let position, duration, content = process subtitle in
    match !current_position with
      | Some (old_position, _) when position <= old_position -> ()
      | Some (old_position, data) ->
          let data = Option.map snd data in
          output ?data ~buffer ~length:(position - old_position) ();
          current_position := Some (position, Some (duration, content))
      | None ->
          (* Output initial silence if the first subtitle doesn't start at 0 *)
          if position > 0 then output ?data:None ~buffer ~length:position ();
          current_position := Some (position, Some (duration, content))
  in
  let flush buffer =
    match !current_position with
      | Some (_, Some (length, content)) ->
          output ?data:(Some content) ~buffer ~length ()
      | _ -> ()
  in
  let decoder ~buffer = function
    | `Flush -> flush buffer
    | `Subtitle subtitle -> process ~buffer subtitle
  in
  { decoder; advance }

module Internal_scaler = Swscale.Make (Swscale.Frame) (Swscale.BigArray)

(* Every decoder to internal content scales through this: the picture keeps its
   proportions as displayed and is centred in the internal frame. *)
let internal_scaler ~width ~height ~pixel_format
    (format : Avutil.Video.frame_format) =
  let fitted_width, fitted_height =
    Ffmpeg_avfilter_utils.Fit.fitted_size ~width ~height format
  in
  let scaler =
    Internal_scaler.create
      ~threads:(Ffmpeg_utils.scaling_threads ())
      [] format.width format.height format.pixel_format fitted_width
      fitted_height pixel_format
  in
  fun frame : Mm.Video.Canvas.Image.t ->
    Internal_scaler.convert scaler frame
    |> Ffmpeg_utils.unpack_image ~width:fitted_width ~height:fitted_height
    |> Mm.Video.Canvas.Image.make
    |> Mm.Video.Canvas.Image.translate
         ((width - fitted_width) / 2)
         ((height - fitted_height) / 2)
    |> Mm.Video.Canvas.Image.viewport width height

type internal_video_stage = {
  format : Avutil.Video.frame_format;
  time_base : Avutil.rational;
  stream_idx : int64;
  scale : Avutil.video Avutil.frame -> Mm.Video.Canvas.Image.t;
  fps : Ffmpeg_avfilter_utils.Fps.t;
}

(** [convert] takes a decoded frame and [flush] the end of the stream; both call
    [put] with each image and the frame it comes from. *)
type internal_video_converter = {
  convert :
    ?stream_idx:int64 ->
    ?format:Avutil.Video.frame_format ->
    time_base:Avutil.rational ->
    put:(Mm.Video.Canvas.Image.t -> Avutil.video Avutil.frame -> unit) ->
    Avutil.video Avutil.frame ->
    unit;
  flush :
    put:(Mm.Video.Canvas.Image.t -> Avutil.video Avutil.frame -> unit) ->
    unit ->
    unit;
}

(* Every decoder to internal content turns its frames into images through
   this. Its scaler and frame rate conversion are built for one stream and one
   frame format: a change of either delivers what they hold, then builds them
   again. *)
let internal_video_converter ~width ~height ~pixel_format () =
  let target_fps = Lazy.Mutexed.force Frame.video_rate in
  let current = ref None in
  let flush ~put () =
    Option.iter
      (fun { scale; fps; _ } ->
        Ffmpeg_avfilter_utils.Fps.eof fps (fun frame -> put (scale frame) frame))
      !current
  in
  let stage ~put ~stream_idx ~time_base format =
    match !current with
      | Some stage
        when stage.stream_idx = stream_idx
             && stage.time_base = time_base
             && Ffmpeg_utils.same_video_format stage.format format ->
          stage
      | previous ->
          flush ~put ();
          if previous <> None then log#important "Video format change.";
          let stage =
            {
              format;
              time_base;
              stream_idx;
              scale =
                internal_scaler ~width ~height
                  ~pixel_format:(pixel_format format) format;
              fps =
                Ffmpeg_avfilter_utils.Fps.of_frame_format ~format ~time_base
                  ~target_fps ();
            }
          in
          current := Some stage;
          stage
  in
  let convert ?(stream_idx = 0L) ?format ~time_base ~put frame =
    let format =
      match format with
        | Some format -> format
        | None -> Avutil.Video.frame_format frame
    in
    let { scale; fps; _ } = stage ~put ~stream_idx ~time_base format in
    Ffmpeg_avfilter_utils.Fps.convert fps frame (fun frame ->
        put (scale frame) frame)
  in
  { convert; flush }
