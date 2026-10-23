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

let per_unit n = { Avutil.num = 1; den = n }

let output_format (output : Producer.output) =
  Frame.Fields.find output.field
    (Generator.content_type output.buffer.generator)

let item_fault faults ~kind message =
  match Faults.item faults ~kind message with
    | `Item_fault -> ()
    | `Source_fault ->
        raise (Producer.Source_fault "too many consecutive decoding faults")

let guarded faults ~kind f =
  try
    f ();
    Faults.delivered faults
  with Avutil.Error e -> item_fault faults ~kind (Avutil.string_of_error e)

(* Input times of the items sent to a decoder, by pts: a decoded item takes
   the input time of the item it came from (inline-codecs.md §3.3). *)
type origins = { times : (Int64.t, int) Hashtbl.t; mutable last : int }

let origins () = { times = Hashtbl.create 16; last = 0 }

let sent origins ~pts ~input_time =
  origins.last <- input_time;
  Option.iter (fun pts -> Hashtbl.replace origins.times pts input_time) pts

let origin origins ~pts =
  match Option.bind pts (Hashtbl.find_opt origins.times) with
    | Some t ->
        Hashtbl.remove origins.times (Option.get pts);
        t
    | None -> origins.last

type same_source = { source_id : Source_id.t; time_base : Avutil.rational }

let same_source a b = a.source_id = b.source_id && a.time_base = b.time_base

(* Planar float at the audio rate, in the output's channel count. *)
let pcm_output (output : Producer.output) =
  let channels = Content.Audio.channels_of_format (output_format output) in
  let layout = Avutil.Channel_layout.get_default channels in
  let rate = Lazy.Mutexed.force Frame.audio_rate in
  let module R =
    Swresample.Make (Swresample.Frame) (Swresample.PlanarFloatArray)
  in
  let resampler = ref None in
  let put data =
    if Mm.Audio.length data > 0 then
      output.buffer.put_pcm ~field:output.field ~samplerate:rate data
  in
  let drain () =
    match !resampler with
      | Some (_, r) ->
          resampler := None;
          put (R.flush r)
      | None -> ()
  in
  let convert frame =
    let format = Avutil.Audio.frame_format frame in
    let r =
      match !resampler with
        | Some (key, r) when Avutil.Audio.same_frame_format key format -> r
        | _ ->
            drain ();
            let r =
              R.create format.channel_layout
                ~in_sample_format:format.sample_format format.sample_rate layout
                rate
            in
            resampler := Some (format, r);
            r
    in
    put (R.convert r frame)
  in
  (convert, drain)

let audio ~(log : Log.t) ~mode : Producer.builder =
 fun output ~input:_ ->
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field output.field in
  let convert, drain_resampler = pcm_output output in
  let origins = origins () in
  let put frame =
    output.place (origin origins ~pts:(Avutil.Frame.pts frame));
    convert frame
  in
  let push, drain =
    match mode with
      | `Copy ->
          let current = ref None in
          let drain_decoder () =
            match !current with
              | Some (_, _, decoder) ->
                  current := None;
                  guarded faults ~kind (fun () ->
                      Avcodec.flush_decoder decoder put)
              | None -> ()
          in
          (* ponytail: an extradata-only change keeps the decoder; codecs that
             take no in-band extradata need a rebuild (inline-codecs.md §10.1),
             which needs a per-codec capability the bindings lack. *)
          let decoder_for source params =
            match !current with
              | Some (s, p, decoder)
                when same_source s source && Copy_params.compatible p params ->
                  decoder
              | previous ->
                  (match previous with
                    | Some (s, p, _) when same_source s source ->
                        log#important "Decoder parameters changed from %s to %s"
                          (Copy_params.to_string p)
                          (Copy_params.to_string params)
                    | _ -> ());
                  drain_decoder ();
                  drain_resampler ();
                  let decoder =
                    match params.Copy_params.media with
                      | `Audio p -> (
                          try
                            Avcodec.Audio.create_decoder ~params:p
                              (Avcodec.Audio.find_decoder
                                 (Avcodec.Audio.get_params_id p))
                          with exn ->
                            raise
                              (Producer.Source_fault
                                 (Printf.sprintf "No decoder for %s: %s"
                                    (Copy_params.to_string params)
                                    (Printexc.to_string exn))))
                      | _ ->
                          raise
                            (Producer.Source_fault "Copy content is not audio")
                  in
                  current := Some (source, params, decoder);
                  decoder
          in
          ( (fun ~input_time data ->
              let content = Ffmpeg_content.Copy.get_data data in
              List.iter
                (fun (start, (run : Ffmpeg_content.Copy.Data.run)) ->
                  let params =
                    match (run.source_params, content.params) with
                      | Some p, _ | None, Some p -> p
                      | None, None ->
                          raise
                            (Producer.Source_fault
                               "Copy content without parameters")
                  in
                  let decoder =
                    decoder_for
                      { source_id = run.source_id; time_base = run.time_base }
                      params
                  in
                  List.iter
                    (fun (position, packet) ->
                      match packet with
                        | `Audio packet ->
                            sent origins
                              ~pts:(Avcodec.Packet.get_pts packet)
                              ~input_time:(input_time + start + position);
                            guarded faults ~kind (fun () ->
                                Avcodec.decode decoder put packet)
                        | _ -> ())
                    run.items)
                content.runs),
            fun () ->
              drain_decoder ();
              drain_resampler () )
      | `Raw ->
          ( (fun ~input_time data ->
              let content = Ffmpeg_content.Audio.get_data data in
              List.iter
                (fun (start, (run : Ffmpeg_content.Audio.Data.run)) ->
                  List.iter
                    (fun (position, frame) ->
                      sent origins ~pts:(Avutil.Frame.pts frame)
                        ~input_time:(input_time + start + position);
                      guarded faults ~kind (fun () -> put frame))
                    run.items)
                content.runs),
            drain_resampler )
  in
  {
    Producer.base = 0;
    push;
    drain;
    discard = (fun () -> ());
    close = (fun () -> ());
  }

type video_stream = {
  source : same_source;
  params : Copy_params.t option;
  decoder : Avutil.video Avcodec.decoder option;
  display : Display.t;
  mutable keyframe_seen : bool;
  mutable waited_from : int option;
  mutable skipped : int;
}

let video ~(log : Log.t) ~mode : Producer.builder =
 fun output ~input:_ ->
  let faults = Faults.create log in
  let kind = Frame.Fields.string_of_field output.field in
  let width, height =
    Content.Video.dimensions_of_format (output_format output)
  in
  let target (format : Avutil.Video.frame_format) =
    if Pixel_formats.has_alpha format.pixel_format then `Yuva420p else `Yuv420p
  in
  let converter =
    Internal_video_converter.create ~log ~width ~height ~target ()
  in
  let fps = { Decoder.num = Lazy.Mutexed.force Frame.video_rate; den = 1 } in
  let origins = origins () in
  let put image frame =
    output.place (origin origins ~pts:(Avutil.Frame.pts frame));
    output.buffer.put_yuva420p ~field:output.field ~fps image
  in
  let current = ref None in
  let to_converter stream frame =
    Internal_video_converter.convert ~source_id:stream.source.source_id
      converter ~time_base:stream.source.time_base ~put frame
  in
  let drain_stream () =
    match !current with
      | Some stream ->
          current := None;
          guarded faults ~kind (fun () ->
              Option.iter
                (fun decoder ->
                  Avcodec.flush_decoder decoder
                    (Display.convert stream.display (to_converter stream)))
                stream.decoder;
              Display.eof stream.display (to_converter stream))
      | None -> ()
  in
  let new_stream source params =
    let decoder =
      match params with
        | None -> None
        | Some (p : Copy_params.t) -> (
            match p.media with
              | `Video vp -> (
                  try
                    Some
                      (Avcodec.Video.create_decoder ~params:vp
                         (Avcodec.Video.find_decoder
                            (Avcodec.Video.get_params_id vp)))
                  with exn ->
                    raise
                      (Producer.Source_fault
                         (Printf.sprintf "No decoder for %s: %s"
                            (Copy_params.to_string p) (Printexc.to_string exn)))
                  )
              | _ -> raise (Producer.Source_fault "Copy content is not video"))
    in
    let display =
      match params with
        | Some { Copy_params.media = `Video vp; _ } ->
            Display.create ~params:vp ~time_base:source.time_base ()
        | _ -> Display.create ~time_base:source.time_base ()
    in
    let stream =
      {
        source;
        params;
        decoder;
        display;
        keyframe_seen = false;
        waited_from = None;
        skipped = 0;
      }
    in
    current := Some stream;
    stream
  in
  let stream_for source params =
    match !current with
      | Some s
        when same_source s.source source
             &&
               match (s.params, params) with
               | Some a, Some b -> Copy_params.compatible a b
               | None, None -> true
               | _ -> false ->
          s
      | previous ->
          (match (previous, params) with
            | Some { source = s; params = Some p; _ }, Some p'
              when same_source s source ->
                log#important "Decoder parameters changed from %s to %s"
                  (Copy_params.to_string p) (Copy_params.to_string p')
            | _ -> ());
          drain_stream ();
          new_stream source params
  in
  let max_wait () = Frame.main_of_seconds Settings.max_keyframe_wait#get in
  (* inline-codecs.md §11.1: a new decoder starts at a keyframe, waiting at
     most [max_keyframe_wait] of input time. *)
  let decodable stream packet ~input_time =
    stream.keyframe_seen
    ||
    let key = List.mem `Keyframe (Avcodec.Packet.get_flags packet) in
    let waited_from = Option.value ~default:input_time stream.waited_from in
    stream.waited_from <- Some waited_from;
    if key || input_time - waited_from > max_wait () then (
      stream.keyframe_seen <- true;
      if stream.skipped > 0 then
        log#important "%d packets skipped %s" stream.skipped
          (if key then "before the first keyframe"
           else "without a keyframe within the wait");
      true)
    else (
      stream.skipped <- stream.skipped + 1;
      false)
  in
  let push ~input_time data =
    match mode with
      | `Copy ->
          let content = Ffmpeg_content.Copy.get_data data in
          List.iter
            (fun (start, (run : Ffmpeg_content.Copy.Data.run)) ->
              let params =
                match run.source_params with
                  | Some p -> Some p
                  | None -> content.params
              in
              let stream =
                stream_for
                  { source_id = run.source_id; time_base = run.time_base }
                  params
              in
              List.iter
                (fun (position, packet) ->
                  match (packet, stream.decoder) with
                    | `Video packet, Some decoder ->
                        let input_time = input_time + start + position in
                        if decodable stream packet ~input_time then (
                          sent origins
                            ~pts:(Avcodec.Packet.get_pts packet)
                            ~input_time;
                          guarded faults ~kind (fun () ->
                              Avcodec.decode decoder
                                (Display.convert stream.display
                                   (to_converter stream))
                                packet))
                    | _ -> ())
                run.items)
            content.runs
      | `Raw ->
          let content = Ffmpeg_content.Video.get_data data in
          List.iter
            (fun (start, (run : Ffmpeg_content.Video.Data.run)) ->
              let stream =
                stream_for
                  { source_id = run.source_id; time_base = run.time_base }
                  None
              in
              List.iter
                (fun (position, frame) ->
                  sent origins ~pts:(Avutil.Frame.pts frame)
                    ~input_time:(input_time + start + position);
                  guarded faults ~kind (fun () ->
                      Display.convert stream.display (to_converter stream) frame))
                run.items)
            content.runs
  in
  let drain () =
    drain_stream ();
    guarded faults ~kind (fun () ->
        Internal_video_converter.flush converter ~put)
  in
  {
    Producer.base = 0;
    push;
    drain;
    discard = (fun () -> ());
    close = (fun () -> ());
  }
