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

module Generator = Generator.From_audio_video

let log = Log.make ["ffmpeg"; "filter"; "bitstream"]
let () = Lang.add_module "ffmpeg.filter.bitstream"

type 'a handler = {
  stream_idx : int64;
  params : 'a Avcodec.params;
  time_base : Avutil.rational;
  duration_converter : 'a Avcodec.Packet.t Ffmpeg_utils.Duration.t;
}

type 'a lift_data =
  ( 'a Avcodec.params option,
    'a Ffmpeg_copy_content.packet )
  Ffmpeg_content_base.content ->
  Content.data

let args_of_args args =
  let opts = Hashtbl.create 10 in
  let rec f = function
    | [] -> ()
    | `Pair (lbl, v) :: args ->
        Hashtbl.add opts lbl v;
        f args
  in
  f args;
  opts

let modes name (codecs : Avcodec.id list) =
  let has_audio =
    List.exists
      (fun codec -> List.mem (codec :> Avcodec.id) codecs)
      Avcodec.Audio.codec_ids
  in
  let has_video =
    List.exists
      (fun codec -> List.mem (codec :> Avcodec.id) codecs)
      Avcodec.Video.codec_ids
  in
  match (codecs, has_audio, has_video) with
    | [], _, _ | _, true, true -> [`Audio; `Video]
    | _, true, false -> [`Audio_only]
    | _, false, true -> [`Video_only]
    | _, false, false ->
        log#important "No valid mode found for filter %s!" name;
        []

let process (type a) ~put_data ~(lift_data : a lift_data) ~generator
    ({ time_base; params; stream_idx } : a handler)
    ((duration, packets) : int * (int * a Avcodec.Packet.t) list) =
  let data =
    List.map
      (fun (pos, packet) ->
        (pos, { Ffmpeg_copy_content.packet; time_base; stream_idx }))
      packets
  in
  let data = { Ffmpeg_content_base.params = Some params; data } in
  let data = lift_data data in
  put_data ?pts:None generator data 0 duration

let flush (type a) ~put_data ~(lift_data : a lift_data) ~generator
    (handler : a handler option) =
  match handler with
    | None -> ()
    | Some h ->
        process ~put_data ~lift_data ~generator h
          (0, Ffmpeg_utils.Duration.flush h.duration_converter)

let on_data (type a)
    ~(get_filter :
       a Avcodec.params -> a Avcodec.BitstreamFilter.t * a Avcodec.params)
    ~(get_handler :
       stream_idx:int64 ->
       time_base:Avutil.rational ->
       a Avcodec.params ->
       a handler) ~put_data ~(lift_data : a lift_data) ~generator
    ({ Ffmpeg_content_base.params; data } :
      ( a Avcodec.params option,
        a Ffmpeg_copy_content.packet )
      Ffmpeg_content_base.content) =
  let filter, params = get_filter (Option.get params) in
  List.iter
    (fun (_, { Ffmpeg_copy_content.stream_idx; time_base; packet }) ->
      let handler = get_handler ~stream_idx ~time_base params in
      Avcodec.BitstreamFilter.send_packet filter packet;
      let rec f () =
        try
          match
            Ffmpeg_utils.Duration.push handler.duration_converter
              (Avcodec.BitstreamFilter.receive_packet filter)
          with
            | None -> f ()
            | Some v ->
                process ~put_data ~lift_data ~generator handler v;
                f ()
        with Avutil.Error `Eagain -> ()
      in
      f ())
    data

let mk_filter ~opts ~filter = Avcodec.BitstreamFilter.init ~opts filter

let get_filter ~opts () =
  let filter = ref None in
  fun params ->
    match !filter with
      | None ->
          let f = mk_filter ~opts params in
          filter := Some f;
          f
      | Some f -> f

let mk_handler (type a) ~stream_idx ~time_base (params : a Avcodec.params) =
  {
    stream_idx;
    params;
    time_base;
    duration_converter =
      Ffmpeg_utils.Duration.init ~src:time_base ~get_ts:Avcodec.Packet.get_dts;
  }

let handler_getters (type a) ~put_data ~(lift_data : a lift_data) ~generator =
  let handler : a handler option ref = ref None in
  let current_handler () : a handler option = !handler in
  let get_handler ~stream_idx ~time_base (params : a Avcodec.params) =
    match !handler with
      | None ->
          let h = mk_handler ~stream_idx ~time_base params in
          handler := Some h;
          (h : a handler)
      | Some h ->
          if h.stream_idx <> stream_idx then (
            flush ~put_data ~lift_data ~generator (Some h);
            let h = mk_handler ~stream_idx ~time_base params in
            handler := Some h;
            (h : a handler))
          else (h : a handler)
  in
  (current_handler, get_handler)

let () =
  List.iter
    (fun ({ Avcodec.BitstreamFilter.name; codecs; options } as filter) ->
      let args, args_parser = Builtins_ffmpeg_filters.mk_options options in
      let modes = modes name codecs in
      if List.length modes > 1 then
        Lang.add_module ("ffmpeg.filter.bitstream." ^ name);
      List.iter
        (fun mode ->
          let name, source_kind =
            match mode with
              | `Audio ->
                  ( name ^ ".audio",
                    Frame.
                      {
                        audio = `Kind Ffmpeg_copy_content.Audio.kind;
                        video = `Any;
                        midi = Frame.none;
                      } )
              | `Audio_only ->
                  ( name,
                    Frame.
                      {
                        audio = `Kind Ffmpeg_copy_content.Audio.kind;
                        video = `Any;
                        midi = Frame.none;
                      } )
              | `Video ->
                  ( name ^ ".video",
                    Frame.
                      {
                        audio = `Any;
                        video = `Kind Ffmpeg_copy_content.Video.kind;
                        midi = Frame.none;
                      } )
              | `Video_only ->
                  ( name,
                    Frame.
                      {
                        audio = `Any;
                        video = `Kind Ffmpeg_copy_content.Video.kind;
                        midi = Frame.none;
                      } )
          in
          let source_t = Lang.kind_type_of_kind_format source_kind in
          let args_t = ("", Lang.source_t source_t, None, None) :: args in
          Lang.add_operator ~category:`FFmpegFilter
            ("ffmpeg.filter.bitstream." ^ name)
            ~descr:
              ("FFmpeg " ^ name
             ^ " bitstream filter. See ffmpeg documentation for more details.")
            ~flags:[`Extra] args_t ~return_t:source_t
            (fun p ->
              let source = List.assoc "" p in

              let generator = Generator.create `Both in

              let opts = args_of_args (args_parser p []) in

              let encode_frame =
                let flush, process =
                  match mode with
                    | `Audio | `Audio_only ->
                        let put_data = Generator.put_audio in
                        let lift_data = Ffmpeg_copy_content.Audio.lift_data in
                        let get_filter = get_filter ~opts ~filter () in
                        let current_handler, get_handler =
                          handler_getters ~lift_data ~put_data ~generator
                        in
                        let flush () =
                          flush ~lift_data ~put_data ~generator
                            (current_handler ())
                        in
                        ( flush,
                          fun frame ->
                            let pos = Frame.position frame in
                            Generator.put_video generator
                              (Content.copy (Frame.video frame))
                              0 pos;
                            on_data ~get_filter ~get_handler ~put_data
                              ~lift_data ~generator
                              (Ffmpeg_copy_content.Audio.get_data
                                 (Content.sub (Frame.audio frame) 0 pos)) )
                    | `Video | `Video_only ->
                        let put_data = Generator.put_video in
                        let lift_data = Ffmpeg_copy_content.Video.lift_data in
                        let get_filter = get_filter ~opts ~filter () in
                        let current_handler, get_handler =
                          handler_getters ~lift_data ~put_data ~generator
                        in
                        let flush () =
                          flush ~lift_data ~put_data ~generator
                            (current_handler ())
                        in
                        ( flush,
                          fun frame ->
                            let pos = Frame.position frame in
                            Generator.put_audio generator
                              (Content.copy (Frame.audio frame))
                              0 pos;
                            on_data ~get_filter ~get_handler ~put_data
                              ~lift_data ~generator
                              (Ffmpeg_copy_content.Video.get_data
                                 (Content.sub (Frame.video frame) 0 pos)) )
                in
                function
                | `Frame frame ->
                    List.iter
                      (fun (pos, m) -> Generator.add_metadata ~pos generator m)
                      (Frame.get_all_metadata frame);
                    List.iter
                      (fun pos -> Generator.add_break ~pos generator)
                      (List.filter
                         (fun x -> x < Lazy.force Frame.size)
                         (Frame.breaks frame));
                    process frame
                | `Flush -> flush ()
              in

              let kind = Kind.of_kind source_kind in
              let consumer =
                new Producer_consumer.consumer
                  ~write_frame:encode_frame ~name:(name ^ ".consumer") ~kind
                  ~source ()
              in
              new Producer_consumer.producer
                ~check_self_sync:false
                ~consumers_val:[Lang.source (consumer :> Source.source)]
                ~kind ~name:(name ^ ".producer") generator))
        modes)
    Avcodec.BitstreamFilter.filters
