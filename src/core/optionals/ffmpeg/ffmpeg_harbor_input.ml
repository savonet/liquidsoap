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

module Queue = Queues.Queue

let log = Log.make ["input"; "harbor"; "dynamic"]
let shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"input.harbor.dynamic shutdown"
    (fun () -> Atomic.set shutdown true)

let audio_stream_to_record (params : Ffmpeg_stream_description.audio_params)
    field =
  Lang.record
    [
      ("field", Lang.string (Frame.Fields.string_of_field field));
      ("type", Lang.string "audio");
      ("codec", Lang.string params.codec_name);
      ("samplerate", Lang.int params.samplerate);
      ("channels", Lang.int params.channels);
      ("channel_layout", Lang.string params.channel_layout);
    ]

let video_stream_to_record (params : Ffmpeg_stream_description.video_params)
    field =
  let frame_rate =
    match params.frame_rate with
      | Some { Avutil.num; den } when den > 0 ->
          Lang.float (float_of_int num /. float_of_int den)
      | _ -> Lang.null
  in
  Lang.record
    [
      ("field", Lang.string (Frame.Fields.string_of_field field));
      ("type", Lang.string "video");
      ("codec", Lang.string params.codec_name);
      ("width", Lang.int params.width);
      ("height", Lang.int params.height);
      ("pixel_format", Lang.string params.pixel_format);
      ("frame_rate", frame_rate);
    ]

let subtitle_stream_to_record
    (params : Ffmpeg_stream_description.subtitle_params) field =
  Lang.record
    [
      ("field", Lang.string (Frame.Fields.string_of_field field));
      ("type", Lang.string "subtitle");
      ("codec", Lang.string params.codec_name);
    ]

let data_stream_to_record (params : Ffmpeg_stream_description.data_params) field
    =
  Lang.record
    [
      ("field", Lang.string (Frame.Fields.string_of_field field));
      ("type", Lang.string "data");
      ("codec", Lang.string params.codec_name);
    ]

let stream_to_record (stream : Ffmpeg_stream_description.stream) =
  match stream.params with
    | `Audio params -> audio_stream_to_record params stream.field
    | `Video params -> video_stream_to_record params stream.field
    | `Subtitle params -> subtitle_stream_to_record params stream.field
    | `Data params -> data_stream_to_record params stream.field

let copy_encoder_of_description ?format ?mime_type
    (desc : Ffmpeg_stream_description.container) =
  let streams =
    List.filter_map
      (fun (stream : Ffmpeg_stream_description.stream) ->
        match stream.params with
          | `Audio _ | `Video _ | `Subtitle _ ->
              Some (stream.field, `Copy `Wait_for_keyframe)
          | `Data _ -> None)
      desc.streams
  in
  let ffmpeg_format =
    {
      Ffmpeg_format.format = (match format with None -> desc.format | v -> v);
      mime_type;
      output = `Stream;
      streams;
      interleaved = `Default;
      metadata = Frame.Metadata.empty;
      opts = Hashtbl.create 0;
    }
  in
  Encoder.Ffmpeg ffmpeg_format

let mime_to_format = function
  | "audio/mpeg" | "audio/mp3" -> Av.Format.find_input_format "mp3"
  | "audio/ogg" | "application/ogg" -> Av.Format.find_input_format "ogg"
  | "audio/flac" -> Av.Format.find_input_format "flac"
  | "audio/aac" | "audio/aacp" -> Av.Format.find_input_format "aac"
  | "video/x-matroska" | "audio/x-matroska" ->
      Av.Format.find_input_format "matroska"
  | "video/webm" | "audio/webm" -> Av.Format.find_input_format "webm"
  | "video/mp4" | "audio/mp4" -> Av.Format.find_input_format "mp4"
  | "video/x-flv" -> Av.Format.find_input_format "flv"
  | "video/mpeg" -> Av.Format.find_input_format "mpegts"
  | _ -> None

class ffmpeg_http_input ~dumpfile ~logfile ~bufferize ~max ~replay_meta
  ~mountpoint ~login ~debug ~timeout ~on_connect () =
  let on_connect_callback = on_connect in
  object (self)
    inherit
      Harbor_input.http_input_base
        ~dumpfile ~logfile ~bufferize ~max ~replay_meta ~login ~debug ~timeout
          ()

    val ffmpeg_container = Atomic.make None
    val remaining = Atomic.make None

    method! remaining =
      match Atomic.get remaining with None -> -1 | Some fn -> fn ()

    initializer
      self#on_relay (fun () ->
          self#open_container;
          self#on_wake_up (fun () -> self#start_feed))

    method private open_container =
      let task =
        {
          Duppy.Task.priority = `Blocking;
          events = [`Delay 0.];
          handler =
            (fun _ ->
              (try self#do_open_container
               with exn ->
                 self#log#severe "Error while opening container: %s!"
                   (Printexc.to_string exn));
              []);
        }
      in
      Duppy.Task.add Tutils.scheduler task

    method private do_open_container =
      (* Open FFmpeg container and call on_connect *)
      let socket = Option.get (Atomic.get relay_socket) in
      let mime = Option.get self#get_mime_type in
      let format = mime_to_format (String.lowercase_ascii mime) in
      let read buf ofs len =
        if Atomic.get shutdown then 0
        else (
          try
            let fd = Harbor.file_descr_of_socket socket in
            Tutils.wait_for ~log:(self#log#info "%s") (`Read fd) timeout;
            relay_read socket buf ofs len
          with
            | Harbor.Retry -> 0
            | Tutils.Exit -> 0
            | exn ->
                log#info "Read error: %s" (Printexc.to_string exn);
                0)
      in
      let opts = Hashtbl.create 10 in
      let container =
        try Av.open_input_stream ?format ~opts read
        with exn ->
          log#info "Failed to open FFmpeg container: %s"
            (Printexc.to_string exn);
          raise Harbor.Unknown_codec
      in
      if Hashtbl.length opts > 0 then
        log#info "Unrecognized FFmpeg options: %s"
          (Ffmpeg_format.string_of_options opts);
      let desc =
        try Ffmpeg_stream_description.container ~url:mountpoint container
        with exn ->
          Av.close container;
          log#info "Failed to analyze container: %s" (Printexc.to_string exn);
          raise Harbor.Unknown_codec
      in
      log#info "Container: %s" (Ffmpeg_stream_description.describe desc);
      Atomic.set ffmpeg_container (Some container);
      let stream_records =
        List.map stream_to_record desc.Ffmpeg_stream_description.streams
      in
      let format_value =
        match desc.Ffmpeg_stream_description.format with
          | Some f -> Lang.string f
          | None -> Lang.null
      in
      let mime_type_value =
        let from_desc =
          Option.bind desc.Ffmpeg_stream_description.format
            Utils.mime_of_container_format
        in
        match (mime, from_desc) with
          | m, _ when String.contains m '/' -> Lang.string m
          | _, Some m -> Lang.string m
          | _ -> Lang.null
      in
      let source_value =
        let base = Lang.source (self :> Source.source) in
        let meth_values =
          List.map
            (fun Lang.{ name; value; _ } -> (name, value self))
            (Harbor_input.meth ())
        in
        let callback_values =
          List.map
            (fun c ->
              let Lang.{ name; value; _ } = Lang_source.callback c in
              (name, value self))
            (Harbor_input.callbacks ())
        in
        Lang.meth base (meth_values @ callback_values)
      in
      let copy_encoder =
        let mime_type =
          match (mime, desc.Ffmpeg_stream_description.format) with
            | m, _ when String.contains m '/' -> Some m
            | _, Some f -> Utils.mime_of_container_format f
            | _ -> None
        in
        Lang.val_fun
          [("", "", Some Lang.null)]
          (fun p ->
            let format =
              Lang.to_valued_option Lang.to_string (List.assoc "" p)
            in
            Lang_encoder.L.format
              (copy_encoder_of_description ?format ?mime_type desc))
      in
      let callback_record =
        Lang.record
          [
            ("uri", Lang.string self#uri);
            ( "query",
              Lang.list
                (List.map
                   (fun (lbl, v) ->
                     Lang.product (Lang.string lbl) (Lang.string v))
                   self#groups) );
            ("format", format_value);
            ("mime_type", mime_type_value);
            ("streams", Lang.list stream_records);
            ("headers", Lang.metadata_list pending_headers);
            ("copy_encoder", copy_encoder);
          ]
      in
      let handler = Lang.apply on_connect_callback [("", callback_record)] in
      ignore (Lang.apply handler [("", source_value)])

    method private register_decoder _ =
      match Atomic.get ffmpeg_container with
        | None -> raise Harbor.Unknown_codec
        | Some container ->
            let ctype = self#content_type in
            let ffmpeg_decoder, get_remaining =
              Ffmpeg_decoder.mk_decoder_record ~ctype
                ~decode_first_metadata:true container
            in
            Atomic.set remaining (Some get_remaining);
            let decoder_buffer = Decoder.mk_buffer ~ctype self#buffer in
            create_decoder <- (fun _ -> (ffmpeg_decoder, decoder_buffer))
  end

let stream_info_t =
  Lang.method_t
    (Lang.optional_method_t Lang.unit_t
       [
         ("samplerate", ([], Lang.int_t), "Audio sample rate.");
         ("channels", ([], Lang.int_t), "Number of audio channels.");
         ("channel_layout", ([], Lang.string_t), "Audio channel layout.");
         ("width", ([], Lang.int_t), "Video width.");
         ("height", ([], Lang.int_t), "Video height.");
         ("pixel_format", ([], Lang.string_t), "Video pixel format.");
         ( "frame_rate",
           ([], Lang.nullable_t Lang.float_t),
           "Video frame rate, null if unknown." );
       ])
    [
      ("field", ([], Lang.string_t), "Stream field.");
      ("type", ([], Lang.string_t), "Stream type.");
      ("codec", ([], Lang.string_t), "Stream codec.");
    ]

let on_connect_t =
  let frame_t = Lang.univ_t () in
  let meth_t =
    List.map (fun m -> (m, `Method)) (Harbor_input.meth ())
    @ List.map
        (fun c -> (Lang_source.callback c, `Callback))
        (Harbor_input.callbacks ())
  in
  let source_t = Lang_source._method_t (Lang.source_t frame_t) meth_t in
  let connection_record_t =
    Lang.record_t
      [
        ("uri", Lang.string_t);
        ("query", Lang.metadata_t);
        ("format", Lang.nullable_t Lang.string_t);
        ("mime_type", Lang.nullable_t Lang.string_t);
        ("streams", Lang.list_t stream_info_t);
        ("headers", Lang.metadata_t);
        ( "copy_encoder",
          Lang.fun_t
            [(true, "", Lang.nullable_t Lang.string_t)]
            (Lang.format_t frame_t) );
      ]
  in
  Lang.fun_t
    [(false, "", connection_record_t)]
    (Lang.fun_t [(false, "", source_t)] Lang.unit_t)

let extra_proto =
  [
    ( "on_connect",
      on_connect_t,
      None,
      Some
        "Callback when a source connects. Called with a connection record \
         containing `uri`, `query`, `format`, `mime_type`, `streams`, \
         `headers` and `copy_encoder`; returns a function that receives the \
         source." );
  ]

let input_harbor_dynamic =
  Lang.add_module ~base:Harbor_input.input_harbor "dynamic"

let _ =
  Lang.add_builtin ~base:input_harbor_dynamic "regexp"
    ~descr:
      "Start a http/icecast receiver server. When a source connects, a new \
       source is created and its description (URI, format, streams, headers) \
       passed to the `on_connect` callback, which returns a function that \
       receives the source. A `copy_encoder` is provided for passthrough \
       remuxing."
    ~category:(`Source (`Input `Active))
    (Harbor_input.proto ~buffer_default:12. Lang.regexp_t @ extra_proto)
    Lang.unit_t
    (fun p ->
      Configure.conf_force_start#set true;
      let {
        Harbor_input.pos;
        mountpoint;
        login;
        debug;
        timeout;
        icy;
        icy_charset;
        meta_charset;
        replay_meta;
        port;
        transport;
        dumpfile;
        logfile;
        bufferize;
        max;
      } =
        Harbor_input.parse_args ~parse_mountpoint:Lang.to_regexp p
      in
      let mountpoint_s = Lang.descr_of_regexp mountpoint in
      let on_connect = List.assoc "on_connect" p in
      let sources = Queue.create () in
      let handler =
        {
          Harbor.relay =
            (fun relay ->
              let s =
                new ffmpeg_http_input
                  ~dumpfile ~logfile ~bufferize ~max ~replay_meta
                  ~mountpoint:mountpoint_s ~login ~debug ~timeout ~on_connect ()
              in
              Queue.push sources (relay.Harbor.uri, s);
              s#on_sleep (fun () ->
                  Queue.filter_out sources (fun (_, s') -> s == s'));
              s#set_id
                (if String.length relay.uri > 1 && relay.uri.[0] = '/' then
                   String.sub relay.uri 1 (String.length relay.uri - 1)
                 else if relay.uri = "" || relay.uri = "/" then
                   "input.harbor.dynamic"
                 else relay.uri);
              s#set_stack (Liquidsoap_lang.Lang_core.pos p);
              s#relay relay);
          login;
          icy_charset;
          meta_charset;
          encode_metadata =
            (fun ~mount m ->
              match
                List.find_opt (fun (m, _) -> m = mount) (Queue.elements sources)
              with
                | Some (_, s) -> s#encode_metadata m
                | None -> ());
          get_mime_type =
            (fun ~mount ->
              match
                List.find_opt (fun (m, _) -> m = mount) (Queue.elements sources)
              with
                | Some (_, s) -> s#get_mime_type
                | None -> None);
        }
      in
      Harbor.add_source ~pos ~transport ~port ~mountpoint ~icy handler;
      Lang.unit)
