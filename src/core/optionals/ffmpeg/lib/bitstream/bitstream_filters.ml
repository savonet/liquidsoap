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

module B = Avcodec.BitstreamFilter

let log = Log.make ["ffmpeg"; "filter"; "bitstream"]
let bitstream = Lang.add_module ~base:Lang_modules.ffmpeg_filter "bitstream"
let kind_t kind = Type.make (Format_type.descr (`Kind kind))

(* §3: the option table of one build. *)
let opts args =
  let table = Hashtbl.create 10 in
  let ground : Avfilter.ground_arg -> Avutil.value = function
    | `String s -> `String s
    | `Int i -> `Int i
    | `Int64 i -> `Int64 i
    | `Float f -> `Float f
    | `Rational { Avutil.num; den } -> `String (Printf.sprintf "%d/%d" num den)
  in
  List.iter
    (function
      | `Pair (key, `Array values) ->
          (* ponytail: the bindings expose no array separator for bitstream
             filter options, so the default ',' is used. *)
          Hashtbl.replace table key
            (`String
               (String.concat ","
                  (List.map
                     (fun v ->
                       match ground v with
                         | `String s -> s
                         | `Int i -> string_of_int i
                         | `Int64 i -> Int64.to_string i
                         | `Float f -> Printf.sprintf "%.17g" f)
                     values)))
      | `Pair (key, (#Avfilter.ground_arg as v)) ->
          Hashtbl.replace table key (ground v)
      | `Flag _ -> ())
    args;
  table

let dts_accessors =
  {
    Duration.get_ts = Avcodec.Packet.get_dts;
    set_ts = Avcodec.Packet.set_dts;
    get_duration = Avcodec.Packet.get_duration;
  }

(* §5: the handler for one source id. *)
type 'm handler = {
  source_id : Source_id.t;
  time_base : Avutil.rational;
  filter : 'm B.t;
  params : Copy_params.t;
  out_time_base : Avutil.rational;
  helper : 'm Avcodec.Packet.t Duration.t;
  mutable last_dts : Int64.t option;
}

let builder ~filter ~args ~wrap ~unwrap ~media_of ~to_media : Producer.builder =
 fun output ~input:_ ->
  let faults = Faults.create log in
  let handler = ref None in
  let checked = ref false in
  let input_times = Queue.create () in
  let format () =
    Frame.Fields.find output.field
      (Generator.content_type output.buffer.generator)
  in
  let put h { Duration.length; items } =
    if length > 0 then (
      (match items with
        | first :: _ -> (
            let rec find () =
              match Queue.peek_opt input_times with
                | Some (p, t) when p == first -> Some t
                | Some _ ->
                    ignore (Queue.pop input_times);
                    find ()
                | None -> None
            in
            match find () with Some t -> output.place t | None -> ())
        | [] -> ());
      Generator.put output.buffer.generator output.field
        (Ffmpeg_content.Copy.lift_data
           (Ffmpeg_content.Copy.Data.of_run (Some h.params)
              (Ffmpeg_content.Copy.Data.run ~source_params:h.params
                 ~source_id:h.source_id ~time_base:h.out_time_base ~length
                 (List.map (fun p -> (0, wrap p)) items)))))
  in
  let receive h ~input_time =
    let rec go () =
      match B.receive_packet h.filter with
        | packet -> (
            Queue.push (packet, input_time) input_times;
            match
              (Avcodec.Packet.get_dts packet, Duration.last_ts h.helper)
            with
              | Some dts, Some last when Int64.to_int dts < last ->
                  ignore
                    (Faults.item faults ~kind:"decreasing dts"
                       (Printf.sprintf
                          "Filtered packet dropped: DTS %Ld after %d" dts last));
                  go ()
              | _ -> (
                  match Duration.push h.helper packet with
                    | `Chunk chunk ->
                        put h chunk;
                        go ()
                    | _ -> go ()))
        | exception Avutil.Error (`Eagain | `Eof) -> ()
    in
    go ()
  in
  (* §6. *)
  let drain () =
    match !handler with
      | None -> ()
      | Some h ->
          handler := None;
          (try
             B.send_eof h.filter;
             receive h ~input_time:0
           with Avutil.Error e ->
             log#important "Draining bitstream filter failed: %s"
               (Avutil.string_of_error e));
          Option.iter (put h) (Duration.flush h.helper)
  in
  let build ~source_id ~time_base params =
    let opts = opts args in
    let filter_instance, out_params =
      try B.init ~opts ~time_base filter (to_media params)
      with Avutil.Error e ->
        raise
          (Producer.Source_fault
             (Printf.sprintf "Bitstream filter %s failed to initialise: %s"
                filter.B.name (Avutil.string_of_error e)))
    in
    if not !checked then (
      checked := true;
      if Hashtbl.length opts > 0 then
        Runtime_error.raise ~pos:[]
          ~message:
            (Printf.sprintf "Unused bitstream filter options: %s"
               (Avutil.string_of_opts opts))
          "ffmpeg");
    let out_params = Copy_params.make (media_of out_params) in
    (try
       Content.merge (format ())
         (Ffmpeg_content.Copy.lift_params (Some out_params))
     with exn ->
       raise
         (Producer.Source_fault
            (Printf.sprintf "Output parameters do not match: %s\nOutput: %s"
               (Printexc.to_string exn)
               (Copy_params.to_string out_params))));
    let out_time_base = B.time_base_out filter_instance in
    let h =
      {
        source_id;
        time_base;
        filter = filter_instance;
        params = out_params;
        out_time_base;
        helper =
          Duration.create
            ~start:(Generator.field_length output.buffer.generator output.field)
            ~convert_ts:false ~src:out_time_base ~faults dts_accessors;
        last_dts = None;
      }
    in
    handler := Some h;
    h
  in
  let handler_for (run : Ffmpeg_content.Copy.Data.run) =
    match (!handler, run.source_params) with
      | Some h, _
        when h.source_id = run.source_id && h.time_base = run.time_base ->
          Some h
      | current, Some params ->
          (match current with
            | Some h when h.source_id = run.source_id ->
                log#important
                  "Time base change within one source, from %s to %s: \
                   rebuilding the bitstream filter."
                  (Time_base.to_string h.time_base)
                  (Time_base.to_string run.time_base)
            | _ -> ());
          drain ();
          Some (build ~source_id:run.source_id ~time_base:run.time_base params)
      | _, None -> None
  in
  let push_packet h ~input_time packet =
    match (Avcodec.Packet.get_dts packet, h.last_dts) with
      | Some dts, Some last when dts < last ->
          ignore
            (Faults.item faults ~kind:"decreasing dts"
               (Printf.sprintf "Packet dropped: DTS %Ld after %Ld" dts last))
      | dts, _ -> (
          if dts <> None then h.last_dts <- dts;
          try
            B.send_packet h.filter packet;
            receive h ~input_time;
            Faults.delivered faults
          with Avutil.Error e -> (
            handler := None;
            match
              Faults.item faults ~kind:"filter"
                (Printf.sprintf "Packet dropped: %s" (Avutil.string_of_error e))
            with
              | `Source_fault ->
                  raise
                    (Producer.Source_fault "too many consecutive filter faults")
              | `Item_fault -> ()))
  in
  let push ~input_time data =
    List.iter
      (fun (start, (run : Ffmpeg_content.Copy.Data.run)) ->
        if run.items <> [] then (
          match handler_for run with
            | None -> (
                match
                  Faults.item faults ~kind:"unknown parameters"
                    "Packets with unknown parameters dropped"
                with
                  | `Source_fault ->
                      raise
                        (Producer.Source_fault "packets with unknown parameters")
                  | `Item_fault -> ())
            | Some h ->
                List.iter
                  (fun (position, packet) ->
                    match unwrap packet with
                      | Some p ->
                          push_packet h
                            ~input_time:(input_time + start + position)
                            p
                      | None -> ())
                  run.items))
      (Ffmpeg_content.Copy.get_data data).runs
  in
  {
    Producer.base = 0;
    push;
    drain;
    discard = (fun () -> handler := None);
    close = (fun () -> ());
  }

let infallible_note =
  " The operator is fallible exactly when its input is. With an infallible \
   input, a persistent data fault is a hard error: wrap the operator in \
   `mksafe` or a `fallback` to keep streaming through it."

let register ~base ~name ~path ~media filter =
  let args = Filter_args.of_options filter.B.options in
  let field =
    match media with
      | `Audio -> Frame.Fields.audio
      | `Video -> Frame.Fields.video
  in
  let row = Lang.univ_t () in
  let input_t = kind_t Ffmpeg_content.Copy.kind in
  let frame field_t =
    Lang.frame_t row (Frame.Fields.add field field_t Frame.Fields.empty)
  in
  let child_frame_type = frame input_t in
  let return_t = frame (kind_t Ffmpeg_content.Copy.kind) in
  let builder p =
    let rendered = Filter_args.render args p in
    match media with
      | `Audio ->
          builder ~filter ~args:rendered
            ~wrap:(fun p -> `Audio p)
            ~unwrap:(function `Audio p -> Some p | _ -> None)
            ~media_of:(fun p -> `Audio p)
            ~to_media:(fun (params : Copy_params.t) ->
              match params.media with
                | `Audio p -> p
                | _ -> raise (Producer.Source_fault "Not an audio stream"))
      | `Video ->
          builder ~filter ~args:rendered
            ~wrap:(fun p -> `Video p)
            ~unwrap:(function `Video p -> Some p | _ -> None)
            ~media_of:(fun p -> `Video p)
            ~to_media:(fun (params : Copy_params.t) ->
              match params.media with
                | `Video p -> p
                | _ -> raise (Producer.Source_fault "Not a video stream"))
  in
  ignore
    (Lang.add_operator ~base name ~category:`FFmpegFilter ~flags:[`Extra]
       ~descr:
         (Printf.sprintf
            "FFmpeg %s bitstream filter. See ffmpeg documentation for more \
             details.%s"
            path infallible_note)
       (List.map
          (fun (arg : Filter_args.arg) ->
            (arg.name, Lang.nullable_t arg.typ, Some Lang.null, arg.doc))
          args
       @ [("", Lang.source_t child_frame_type, None, None)])
       ~return_t
       (fun p ->
         let builder = builder p in
         Producer.create ~passthrough:true ~child_frame_type
           ~name:("ffmpeg.filter.bitstream." ^ path)
           ~field ~input_t ~check:None ~builder (List.assoc "" p)))

let is_audio (id : Avcodec.id) =
  List.exists (fun a -> (a :> Avcodec.id) = id) Avcodec.Audio.codec_ids

let is_video (id : Avcodec.id) =
  List.exists (fun v -> (v :> Avcodec.id) = id) Avcodec.Video.codec_ids

(* §1. *)
let () =
  List.iter
    (fun (filter : B.filter) ->
      let audio = List.exists is_audio filter.codecs in
      let video = List.exists is_video filter.codecs in
      match (filter.codecs, audio, video) with
        | [], _, _ | _, true, true ->
            let base = Lang.add_module ~base:bitstream filter.name in
            register ~base ~name:"audio" ~path:(filter.name ^ ".audio")
              ~media:`Audio filter;
            register ~base ~name:"video" ~path:(filter.name ^ ".video")
              ~media:`Video filter
        | _, true, false ->
            register ~base:bitstream ~name:filter.name ~path:filter.name
              ~media:`Audio filter
        | _, false, true ->
            register ~base:bitstream ~name:filter.name ~path:filter.name
              ~media:`Video filter
        | _ -> log#info "No valid mode found for filter %s!" filter.name)
    B.filters
