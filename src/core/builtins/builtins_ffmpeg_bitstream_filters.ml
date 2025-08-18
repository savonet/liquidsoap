(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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

open Builtins_ffmpeg_base

let log = Log.make ["ffmpeg"; "filter"; "bitstream"]
let ffmpeg_filter_bitstream = Lang.add_module ~base:ffmpeg_filter "bitstream"

type 'a handler = {
  stream_idx : int64;
  filter : 'a Avcodec.BitstreamFilter.t;
  params : 'a Avcodec.params;
  time_base : Avutil.rational;
  duration_converter : 'a Avcodec.Packet.t Ffmpeg_utils.Duration.t;
}

type 'a get_params = Ffmpeg_copy_content.params_payload -> 'a Avcodec.params
type 'a mk_params = 'a Avcodec.params -> Ffmpeg_copy_content.params_payload
type 'a get_packet = Ffmpeg_copy_content.packet -> 'a Avcodec.Packet.t
type 'a mk_packet = 'a Avcodec.Packet.t -> Ffmpeg_copy_content.packet

let args_of_args args =
  let opts = Hashtbl.create 10 in
  let rec f = function
    | [] -> ()
    | `Pair (lbl, v) :: args ->
        Hashtbl.replace opts lbl v;
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

let process (type a) ~put_data ~(mk_params : a mk_params)
    ~(mk_packet : a mk_packet) ({ time_base; params; stream_idx } : a handler)
    ((length, packets) : int * (int * a Avcodec.Packet.t) list) =
  let data =
    List.map
      (fun (pos, packet) ->
        ( pos,
          {
            Ffmpeg_copy_content.packet = mk_packet packet;
            time_base;
            stream_idx;
          } ))
      packets
  in
  let data =
    { Ffmpeg_copy_content.params = Some (mk_params params); data; length }
  in
  let data = Ffmpeg_copy_content.lift_data data in
  put_data data

let flush_filter ~put_data ~mk_params ~mk_packet handler =
  let rec f () =
    match
      Ffmpeg_utils.Duration.push handler.duration_converter
        (Avcodec.BitstreamFilter.receive_packet handler.filter)
    with
      | None -> f ()
      | Some v ->
          process ~put_data ~mk_params ~mk_packet handler v;
          f ()
      | (exception Avutil.Error `Eagain) | (exception Avutil.Error `Eof) -> ()
  in
  f ()

let flush (type a) ~put_data ~(mk_params : a mk_params)
    ~(mk_packet : a mk_packet) (handler : a handler option) =
  match handler with
    | None -> ()
    | Some h ->
        Avcodec.BitstreamFilter.send_eof h.filter;
        flush_filter ~put_data ~mk_params ~mk_packet h;
        process ~put_data ~mk_params ~mk_packet h
          (0, Ffmpeg_utils.Duration.flush h.duration_converter)

let on_data (type a)
    ~(get_handler :
       put_data:(Content.data -> unit) ->
       stream_idx:int64 ->
       time_base:Avutil.rational ->
       a Avcodec.params ->
       a handler) ~put_data ~(get_params : a get_params)
    ~(get_packet : a get_packet) ~(mk_params : a mk_params)
    ~(mk_packet : a mk_packet)
    ({ Ffmpeg_copy_content.params; data } : Ffmpeg_copy_content.data) =
  List.iter
    (fun (_, { Ffmpeg_copy_content.stream_idx; time_base; packet }) ->
      let handler =
        get_handler ~put_data ~stream_idx ~time_base
          (get_params (Option.get params))
      in
      Avcodec.BitstreamFilter.send_packet handler.filter (get_packet packet);
      flush_filter ~put_data ~mk_packet ~mk_params handler)
    data

let mk_filter ~opts ~filter = Avcodec.BitstreamFilter.init ~opts filter

let mk_handler (type a) ~stream_idx ~time_base
    ~(filter : a Avcodec.BitstreamFilter.t) (params : a Avcodec.params) =
  {
    stream_idx;
    filter;
    params;
    time_base;
    duration_converter =
      Ffmpeg_utils.Duration.init ~mode:`DTS ~src:time_base ~convert_ts:false
        ~get_ts:Avcodec.Packet.get_dts ~set_ts:Avcodec.Packet.set_dts ();
  }

let handler_getters (type a) ~(mk_params : a mk_params)
    ~(mk_packet : a mk_packet) ~filter ~filter_opts =
  let handler : a handler option Atomic.t = Atomic.make None in
  let current_handler () : a handler option = Atomic.get handler in
  let clear_handler () = Atomic.set handler None in
  let get_handler ~put_data ~stream_idx ~time_base (params : a Avcodec.params) =
    match Atomic.get handler with
      | None ->
          let filter, params = mk_filter ~filter ~opts:filter_opts params in
          let h = mk_handler ~stream_idx ~time_base ~filter params in
          Atomic.set handler (Some h);
          (h : a handler)
      | Some h ->
          if h.stream_idx <> stream_idx then (
            flush ~put_data ~mk_params ~mk_packet (Some h);
            let filter, params = mk_filter ~filter ~opts:filter_opts params in
            let h = mk_handler ~stream_idx ~time_base ~filter params in
            Atomic.set handler (Some h);
            (h : a handler))
          else (h : a handler)
  in
  (current_handler, get_handler, clear_handler)

let register_filters () =
  List.iter
    (fun ({ Avcodec.BitstreamFilter.name; codecs; options } as filter) ->
      let args, args_parser = Builtins_ffmpeg_filters.mk_options options in
      let modes = modes name codecs in
      let base, module_name =
        if List.length modes > 1 then
          (Lang.add_module ~base:ffmpeg_filter_bitstream name, [name])
        else (ffmpeg_filter_bitstream, [])
      in
      List.iter
        (fun mode ->
          let typ =
            Type.make
              (Format_type.descr
                 (`Format (Content.default_format Ffmpeg_copy_content.kind)))
          in
          let name, field, source_fields_t =
            match mode with
              | `Audio ->
                  ("audio", Frame.Fields.audio, Frame.Fields.make ~audio:typ ())
              | `Audio_only ->
                  (name, Frame.Fields.audio, Frame.Fields.make ~audio:typ ())
              | `Video ->
                  ("video", Frame.Fields.video, Frame.Fields.make ~video:typ ())
              | `Video_only ->
                  (name, Frame.Fields.video, Frame.Fields.make ~video:typ ())
          in
          let source_t = Lang.frame_t (Lang.univ_t ()) source_fields_t in
          let args_t = ("", Lang.source_t source_t, None, None) :: args in
          ignore
            (Lang.add_operator ~category:`FFmpegFilter name ~base
               ~descr:
                 ("FFmpeg "
                 ^ String.concat "." (module_name @ [name])
                 ^ " bitstream filter. See ffmpeg documentation for more \
                    details.")
               ~flags:[`Extra] args_t ~return_t:source_t
               (fun p ->
                 let source = Lang.to_source (List.assoc "" p) in

                 let filter_opts = args_of_args (args_parser p []) in

                 let flush, process =
                   match mode with
                     | `Audio | `Audio_only ->
                         let mk_packet p = `Audio p in
                         let get_packet = function
                           | `Audio p -> p
                           | _ -> assert false
                         in
                         let mk_params p = `Audio p in
                         let get_params = function
                           | `Audio p -> p
                           | _ -> assert false
                         in
                         let current_handler, get_handler, clear_handler =
                           handler_getters ~mk_packet ~mk_params ~filter
                             ~filter_opts
                         in
                         let flush ~put_data =
                           flush ~put_data ~mk_packet ~mk_params
                             (current_handler ());
                           clear_handler ()
                         in
                         ( flush,
                           on_data ~get_handler ~get_params ~get_packet
                             ~mk_packet ~mk_params )
                     | `Video | `Video_only ->
                         let mk_packet p = `Video p in
                         let get_packet = function
                           | `Video p -> p
                           | _ -> assert false
                         in
                         let mk_params params =
                           `Video
                             {
                               Ffmpeg_copy_content.avg_frame_rate = None;
                               params;
                             }
                         in
                         let get_params = function
                           | `Video { Ffmpeg_copy_content.params } -> params
                           | _ -> assert false
                         in
                         let current_handler, get_handler, clear_handler =
                           handler_getters ~mk_packet ~mk_params ~filter
                             ~filter_opts
                         in
                         let flush ~put_data =
                           flush ~put_data ~mk_packet ~mk_params
                             (current_handler ());
                           clear_handler ()
                         in
                         ( flush,
                           on_data ~get_handler ~get_params ~get_packet
                             ~mk_packet ~mk_params )
                 in

                 let encode_frame generator =
                   let put_data = Generator.put generator field in

                   function
                   | `Frame frame ->
                       List.iter
                         (fun (pos, m) ->
                           Generator.add_metadata ~pos generator m)
                         (Frame.get_all_metadata frame);
                       List.iter
                         (fun pos -> Generator.add_track_mark ~pos generator)
                         (List.filter
                            (fun x -> x < Lazy.force Frame.size)
                            (Frame.track_marks frame));
                       Frame.Fields.iter
                         (fun f data ->
                           match f with
                             | _ when f = Frame.Fields.metadata -> ()
                             | _ when f = Frame.Fields.track_marks -> ()
                             | _ when f = field ->
                                 process ~put_data
                                   (Ffmpeg_copy_content.get_data data)
                             | _ -> Generator.put generator f data)
                         frame
                   | `Flush -> flush ~put_data
                 in

                 new Simple_encoder.encoder ~name ~encode:encode_frame source)))
        modes)
    Avcodec.BitstreamFilter.filters

let () = Startup.time "FFmpeg bitstream filters registration" register_filters
