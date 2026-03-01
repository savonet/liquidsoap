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

let log = Log.make ["input"; "harbor"; "dynamic"]
let shutdown = Atomic.make false

let () =
  Lifecycle.before_core_shutdown ~name:"input.harbor.dynamic shutdown"
    (fun () -> Atomic.set shutdown true)

let normalize_metadata =
  List.map (fun (lbl, v) ->
      let lbl =
        match lbl with
          | "StreamTitle" -> "title"
          | "StreamUrl" -> "url"
          | _ -> lbl
      in
      let v = try Charset.convert ~target:Charset.utf8 v with _ -> v in
      (lbl, v))

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

let copy_encoder_of_description (desc : Ffmpeg_stream_description.container) =
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
      Ffmpeg_format.format = desc.format;
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
  ~mountpoint ~login ~debug ~timeout ~on_connection () =
  object (self)
    inherit
      Harbor_input.http_input_base
        ~dumpfile ~logfile ~bufferize ~max ~replay_meta ~login ~debug ~timeout
          ()

    val closed = Atomic.make false
    val mutable ffmpeg_container = None
    val mutable description = None
    val mutable last_meta = []

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
              self#do_open_container;
              []);
        }
      in
      Duppy.Task.add Tutils.scheduler task

    method private do_open_container =
      (* Open FFmpeg container and call on_connection *)
      let socket = Option.get (Atomic.get relay_socket) in
      let mime = Option.get self#get_mime_type in
      let format = mime_to_format (String.lowercase_ascii mime) in
      let read buf ofs len =
        if Atomic.get closed || Atomic.get shutdown then 0
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
      ffmpeg_container <- Some container;
      description <- Some desc;
      let stream_records =
        List.map stream_to_record desc.Ffmpeg_stream_description.streams
      in
      let format_value =
        match desc.Ffmpeg_stream_description.format with
          | Some f -> Lang.string f
          | None -> Lang.null
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
        Lang_encoder.L.format (copy_encoder_of_description desc)
      in
      let callback_record =
        Lang.record
          [
            ("source", source_value);
            ("uri", Lang.string self#uri);
            ( "query",
              Lang.list
                (List.map
                   (fun (lbl, v) ->
                     Lang.product (Lang.string lbl) (Lang.string v))
                   self#groups) );
            ("format", format_value);
            ("streams", Lang.list stream_records);
            ("headers", Lang.metadata_list pending_headers);
            ("copy_encoder", copy_encoder);
          ]
      in
      ignore (Lang.apply on_connection [("", callback_record)])

    method private register_decoder _ =
      match ffmpeg_container with
        | None -> raise Harbor.Unknown_codec
        | Some container ->
            let ctype = self#content_type in
            let pending_operation = Atomic.make `None in
            let streams =
              Ffmpeg_decoder.mk_streams ~ctype ~decode_first_metadata:true
                container
            in
            let ffmpeg_decoder =
              Ffmpeg_decoder.mk_decoder ~pending_operation ~streams
                ~target_position:(Atomic.make None) container
            in
            let decoder_buffer = Decoder.mk_buffer ~ctype self#buffer in
            let get_metadata stream =
              let m = Av.get_metadata stream in
              Av.set_metadata stream [];
              m
            in
            let get_all_metadata () =
              normalize_metadata
                (Ffmpeg_decoder.Streams.fold
                   (fun _ s m ->
                     m
                     @
                       match s.Ffmpeg_decoder.decoder with
                       | `Audio_frame (stream, _) -> get_metadata stream
                       | `Audio_packet (stream, _) -> get_metadata stream
                       | `Video_frame (stream, _) -> get_metadata stream
                       | `Video_packet (stream, _) -> get_metadata stream
                       | `Subtitle_packet (stream, _) -> get_metadata stream
                       | `Subtitle_frame (stream, _) -> get_metadata stream
                       | `Data_packet _ -> [])
                   streams
                   (match ffmpeg_container with
                     | Some c -> Av.get_input_metadata c
                     | None -> []))
            in
            let decode buf =
              ffmpeg_decoder buf;
              let meta = get_all_metadata () in
              if meta <> last_meta then begin
                last_meta <- meta;
                if meta <> [] then
                  Generator.add_metadata self#buffer
                    (Frame.Metadata.from_list meta)
              end
            in
            let close () =
              Atomic.set closed true;
              match ffmpeg_container with
                | Some c ->
                    (try Av.close c
                     with exn ->
                       log#info "Error closing FFmpeg container: %s"
                         (Printexc.to_string exn));
                    ffmpeg_container <- None
                | None -> ()
            in
            create_decoder <-
              (fun _ ->
                ( {
                    Decoder.decode;
                    eof = (fun _ -> ());
                    seek = (fun _ -> 0);
                    close;
                  },
                  decoder_buffer ))
  end

let stream_info_t =
  Lang.record_t
    [
      ("field", Lang.string_t); ("type", Lang.string_t); ("codec", Lang.string_t);
    ]

let callback_record_t =
  let frame_t = Lang.univ_t () in
  let meth_t =
    List.map (fun m -> (m, `Method)) (Harbor_input.meth ())
    @ List.map
        (fun c -> (Lang_source.callback c, `Callback))
        (Harbor_input.callbacks ())
  in
  let source_t = Lang_source._method_t (Lang.source_t frame_t) meth_t in
  Lang.record_t
    [
      ("source", source_t);
      ("uri", Lang.string_t);
      ("query", Lang.metadata_t);
      ("format", Lang.nullable_t Lang.string_t);
      ("streams", Lang.list_t stream_info_t);
      ("headers", Lang.metadata_t);
      ("copy_encoder", Lang.format_t frame_t);
    ]

let extra_proto =
  [
    ( "on_connection",
      Lang.fun_t [(false, "", callback_record_t)] Lang.unit_t,
      None,
      Some
        "Callback when a source connects. Receives a record with: `source`, \
         `uri`, `query`, `format`, `streams`, `headers` and `copy_encoder`." );
  ]

let input_harbor_dynamic =
  Lang.add_module ~base:Harbor_input.input_harbor "dynamic"

let _ =
  Lang.add_builtin ~base:input_harbor_dynamic "regexp"
    ~descr:
      "Start a http/icecast receiver server. When a source connects, a new \
       source is created and passed to the `on_connection` callback along with \
       a description of its content (URI, format, streams, headers) and a \
       `copy_encoder` that can be used to re-encode the stream as a copy."
    ~category:(`Source `Input)
    (Harbor_input.proto ~buffer_default:0.2 Lang.regexp_t @ extra_proto)
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
      let on_connection = List.assoc "on_connection" p in
      let current_source = Atomic.make None in
      let handler =
        {
          Harbor.relay =
            (fun relay ->
              let s =
                new ffmpeg_http_input
                  ~dumpfile ~logfile ~bufferize ~max ~replay_meta
                  ~mountpoint:mountpoint_s ~login ~debug ~timeout ~on_connection
                  ()
              in
              s#set_id relay.uri;
              Atomic.set current_source (Some s);
              s#set_stack (Liquidsoap_lang.Lang_core.pos p);
              s#relay relay);
          login;
          icy_charset;
          meta_charset;
          encode_metadata =
            (fun m ->
              match Atomic.get current_source with
                | Some s -> s#encode_metadata m
                | None -> ());
          get_mime_type =
            (fun () ->
              match Atomic.get current_source with
                | Some s -> s#get_mime_type
                | None -> None);
        }
      in
      Harbor.add_source ~pos ~transport ~port ~mountpoint ~icy handler;
      Lang.unit)
