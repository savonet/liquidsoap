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

(** HLS output. *)

let log = Log.make ["hls"; "output"]

let x_version = 3

let hls_proto kind =
  let segment_name_t =
    Lang.fun_t
      [ (false, "position", Lang.int_t);
        (false, "extname", Lang.string_t);
        (false, "", Lang.string_t) ]
      Lang.string_t
  in
  let default_name =
    Lang.val_fun
      [ ("position", "position", Lang.int_t, None);
        ("extname", "extname", Lang.string_t, None);
        ("", "", Lang.string_t, None) ] ~ret_t:Lang.string_t (fun p _ ->
        let position = Lang.to_int (List.assoc "position" p) in
        let extname = Lang.to_string (List.assoc "extname" p) in
        let sname = Lang.to_string (List.assoc "" p) in
        Lang.string (Printf.sprintf "%s_%d.%s" sname position extname))
  in
  let stream_info_t =
    Lang.product_t Lang.string_t
      (Lang.tuple_t [Lang.int_t; Lang.string_t; Lang.string_t])
  in
  Output.proto
  @ [ ( "playlist",
        Lang.string_t,
        Some (Lang.string "stream.m3u8"),
        Some "Playlist name (m3u8 extension is recommended)." );
      ( "segment_duration",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Segment duration (in seconds)." );
      ( "segment_name",
        segment_name_t,
        Some default_name,
        Some
          "Segment name. Default: `fun (~position,~extname,stream_name) -> \
           \"#{stream_name}_#{position}.#{extname}\"`" );
      ( "segments_overhead",
        Lang.int_t,
        Some (Lang.int 5),
        Some
          "Number of segments to keep after they have been featured in the \
           live playlist." );
      ( "segments",
        Lang.int_t,
        Some (Lang.int 10),
        Some "Number of segments per playlist." );
      ( "encode_metadata",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Insert metadata into encoded stream. Note: Some HLS players (in \
           particular android native HLS player) expect a single mpegts \
           stream. Encoding metadata will break that assumption." );
      ( "perm",
        Lang.int_t,
        Some (Lang.int 0o644),
        Some
          "Permission of the created files, up to umask. You can and should \
           write this number in octal notation: 0oXXX. The default value is \
           however displayed in decimal (0o666 = 6*8^2 + 4*8 + 4 = 412)." );
      ( "on_file_change",
        Lang.fun_t
          [(false, "state", Lang.string_t); (false, "", Lang.string_t)]
          Lang.unit_t,
        Some
          (Lang.val_cst_fun
             [("state", Lang.string_t, None); ("", Lang.string_t, None)]
             Lang.unit),
        Some
          "Callback executed when a file changes. `state` is one of: \
           `\"opened\"`, `\"closed\"` or `\"deleted\"`, second argument is \
           file path. Typical use: upload files to a CDN when done writting \
           (`\"close\"` state and remove when `\"deleted\"`." );
      ( "streams_info",
        Lang.list_t stream_info_t,
        Some (Lang.list ~t:stream_info_t []),
        Some
          "Additional information about the streams. Should be a list of the \
           form: `[(stream_name, (bandwidth, codec, extname)]`. See RFC 6381 \
           for info about codec. Stream info are required when they cannot be \
           inferred from the encoder." );
      ( "persist",
        Lang.bool_t,
        Some (Lang.bool false),
        Some
          "Persist output accross restart. If enabled, generated files \
           (segments and playlists) are kept on shutdown and the output state \
           at the location given by `persist_at` and used on restart." );
      ( "persist_at",
        Lang.string_t,
        Some (Lang.string "state.config"),
        Some
          "Location of the configuration file used to restart the output when \
           `persist=true`. Relative paths are assumed to be with regard to \
           the directory for generated file." );
      ("", Lang.string_t, None, Some "Directory for generated files.");
      ( "",
        Lang.list_t (Lang.product_t Lang.string_t (Lang.format_t kind)),
        None,
        Some "List of specifications for each stream: (name, format)." );
      ("", Lang.source_t kind, None, None) ]

type segment = {
  id: int;
  discontinuous: bool;
  discontinuity_count: int;
  files: (string * string) List.t;
  mutable len: int;
}

(** A stream in the HLS (which typically contains many, with different qualities). *)
type hls_stream_desc = {
  hls_name: string;  (** name of the stream *)
  hls_format: Encoder.format;
  hls_encoder: Encoder.encoder;
  hls_bandwidth: int;
  hls_codec: string;  (** codec (see RFC 6381) *)
  hls_extname: string;
  mutable hls_oc: (string * out_channel) option;  (** currently encoded file *)
}

open Extralib

let ( ^^ ) = Filename.concat

type file_state = [`Opened | `Closed | `Deleted]

let string_of_file_state = function
  | `Opened ->
      "opened"
  | `Closed ->
      "closed"
  | `Deleted ->
      "deleted"

class hls_output p =
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let encode_metadata = Lang.to_bool (List.assoc "encode_metadata" p) in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_file_change =
    let f = List.assoc "on_file_change" p in
    fun ~state fname ->
      ignore
        (Lang.apply ~t:Lang.unit_t f
           [ ("state", Lang.string (string_of_file_state state));
             ("", Lang.string fname) ])
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let directory = Lang.to_string (Lang.assoc "" 1 p) in
  let () =
    if (not (Sys.file_exists directory)) || not (Sys.is_directory directory)
    then
      raise
        (Lang_errors.Invalid_value
           (Lang.assoc "" 1 p, "The target directory does not exist"))
  in
  let persist = Lang.to_bool (List.assoc "persist" p) in
  let persist_at =
    let p = Lang.to_string (List.assoc "persist_at" p) in
    if Filename.is_relative p then Filename.concat directory p else p
  in
  let () =
    if persist then (
      let dir = Filename.dirname persist_at in
      try Utils.mkdir ~perm:0o777 dir
      with exn ->
        raise
          (Lang_errors.Invalid_value
             ( Lang.assoc "persist_at" 1 p,
               Printf.sprintf
                 "Error while creating directory %S for persisting state: %s"
                 dir (Printexc.to_string exn) )) )
  in
  let streams_info =
    let streams_info = List.assoc "streams_info" p in
    let l = Lang.to_list streams_info in
    List.map
      (fun el ->
        let name, specs = Lang.to_product el in
        let bandwidth, codec, extname =
          match Lang.to_tuple specs with
            | [bandwidth; codec; extname] ->
                (bandwidth, codec, extname)
            | _ ->
                assert false
        in
        ( Lang.to_string name,
          (Lang.to_int bandwidth, Lang.to_string codec, Lang.to_string extname)
        ))
      l
  in
  let streams =
    let streams = Lang.assoc "" 2 p in
    let l = Lang.to_list streams in
    if l = [] then
      raise
        (Lang_errors.Invalid_value
           (streams, "The list of streams cannot be empty")) ;
    l
  in
  let streams =
    let f s =
      let name, fmt = Lang.to_product s in
      let hls_name = Lang.to_string name in
      let hls_format = Lang.to_format fmt in
      let hls_encoder_factory =
        try Encoder.get_factory hls_format
        with Not_found ->
          raise (Lang_errors.Invalid_value (fmt, "Unsupported format"))
      in
      let hls_encoder =
        hls_encoder_factory hls_name Meta_format.empty_metadata
      in
      let hls_bandwidth, hls_codec, hls_extname =
        try List.assoc hls_name streams_info
        with Not_found ->
          let hls_bandwidth =
            try Encoder.bitrate hls_format
            with Not_found ->
              raise
                (Lang_errors.Invalid_value
                   ( fmt,
                     "Bandwidth cannot be inferred from codec, please specify \
                      it in `streams_info`" ))
          in
          let hls_codec =
            try Encoder.iso_base_file_media_file_format hls_format
            with Not_found ->
              raise
                (Lang_errors.Invalid_value
                   ( fmt,
                     "Codec cannot be inferred from codec, please specify it \
                      in `streams_info`" ))
          in
          let hls_extname =
            try Encoder.extension hls_format
            with Not_found ->
              raise
                (Lang_errors.Invalid_value
                   ( fmt,
                     "File extension cannot be inferred from codec, please \
                      specify it in `streams_info`" ))
          in
          (hls_bandwidth, hls_codec, hls_extname)
      in
      {
        hls_name;
        hls_format;
        hls_encoder;
        hls_bandwidth;
        hls_codec;
        hls_extname;
        hls_oc= None;
      }
    in
    let streams = List.map f streams in
    streams
  in
  let source = Lang.assoc "" 3 p in
  let playlist = Lang.to_string (List.assoc "playlist" p) in
  let name = playlist in
  (* better choice? *)
  let segment_duration = Lang.to_float (List.assoc "segment_duration" p) in
  let segment_ticks =
    Frame.master_of_seconds segment_duration / Lazy.force Frame.size
  in
  let segment_master_duration = segment_ticks * Lazy.force Frame.size in
  let segment_duration = Frame.seconds_of_master segment_master_duration in
  let segment_name =
    Lang.to_fun ~t:Lang.string_t (List.assoc "segment_name" p)
  in
  let segment_name ~position ~extname sname =
    Lang.to_string
      (segment_name
         [ ("position", Lang.int position);
           ("extname", Lang.string extname);
           ("", Lang.string sname) ])
  in
  let segment_names id =
    List.map
      (fun {hls_name; hls_extname} ->
        let fname = segment_name ~position:id ~extname:hls_extname hls_name in
        (hls_name, fname))
      streams
  in
  let segments_per_playlist = Lang.to_int (List.assoc "segments" p) in
  let max_segments =
    segments_per_playlist + Lang.to_int (List.assoc "segments_overhead" p)
  in
  let file_perm = Lang.to_int (List.assoc "perm" p) in
  let kind = Encoder.kind_of_format (List.hd streams).hls_format in
  object (self)
    inherit
      Output.encoded
        ~infallible ~on_start ~on_stop ~autostart ~output_kind:"output.file"
          ~name ~content_kind:kind source

    (** Current segment ID *)
    val mutable current_segment =
      {
        id= 0;
        files= segment_names 0;
        discontinuous= false;
        discontinuity_count= 0;
        len= 0;
      }

    (** Available segments *)
    val mutable segments = Queue.create ()

    (** Opening date for current segment. *)
    val mutable open_tick = 0

    val mutable current_metadata = None

    val mutable state = `Idle

    method private toggle_state event =
      match (event, state) with
        | `Restart, _ | `Start, `Resumed | `Start, `Stopped ->
            state <- `Restarted
        | `Stop, _ ->
            state <- `Stopped
        | `Start, _ ->
            state <- `Started
        | `Streaming, _ ->
            state <- `Streaming
        | `Resumed, _ ->
            state <- `Resumed

    method private segment_name ?(relative = false) ~segment stream =
      let fname = List.assoc stream.hls_name segment.files in
      (if relative then "" else directory) ^^ fname

    method private open_out fname =
      let mode = [Open_wronly; Open_creat; Open_trunc] in
      let oc = open_out_gen mode file_perm fname in
      set_binary_mode_out oc true ;
      on_file_change ~state:`Opened fname ;
      oc

    method private unlink fname =
      on_file_change ~state:`Deleted fname ;
      try Unix.unlink fname
      with Unix.Unix_error (e, _, _) ->
        (self#log)#important "Could not remove file %s: %s" fname
          (Unix.error_message e)

    method private unlink_segment segment =
      (self#log)#debug "Cleaning up segment %d.." segment.id ;
      List.iter (fun s -> self#unlink (self#segment_name ~segment s)) streams

    method private close_out (fname, oc) =
      close_out oc ;
      on_file_change ~state:`Closed fname

    method private close_segment s =
      match s.hls_oc with
        | None ->
            ()
        | Some v ->
            self#close_out v ;
            s.hls_oc <- None

    method private open_segment s =
      let meta =
        match current_metadata with
          | Some m ->
              m
          | None ->
              Meta_format.empty_metadata
      in
      if encode_metadata then s.hls_encoder.Encoder.insert_metadata meta ;
      let fname = self#segment_name ~segment:current_segment s in
      let oc = self#open_out fname in
      s.hls_oc <- Some (fname, oc) ;
      Strings.iter (output_substring oc) s.hls_encoder.Encoder.header

    method private push_current_segment =
      List.iter (fun s -> self#close_segment s) streams ;
      Queue.push current_segment segments ;
      if Queue.length segments >= max_segments then
        self#unlink_segment (Queue.take segments) ;
      self#write_playlists ;
      let id = current_segment.id + 1 in
      let len = 0 in
      let files = segment_names id in
      (* Discontinuity is bumped each time a discontinuous
       * segment exits the playlist. We keep track of it
       * here. Any [current_segment] may be mark as discontinuous
       * during the next call to [self#new_segment], in which
       * case [discontinuity_count] is bumped in the following
       * segment. *)
      let discontinuity_count =
        if current_segment.discontinuous then
          current_segment.discontinuity_count + 1
        else current_segment.discontinuity_count
      in
      current_segment <-
        {discontinuous= false; id; files; len; discontinuity_count}

    method private new_segment =
      current_segment <-
        {current_segment with discontinuous= state = `Restarted} ;
      self#toggle_state `Streaming ;
      open_tick <- self#current_tick ;
      (self#log)#debug "Opening segment %d." current_segment.id ;
      List.iter (fun s -> self#open_segment s) streams

    method private current_tick =
      if Source.Clock_variables.is_known self#clock then
        (Source.Clock_variables.get self#clock)#get_tick
      else 0

    method private write_pipe s b =
      let _, oc = Utils.get_some s.hls_oc in
      Strings.iter (output_substring oc) b

    method private cleanup_segments =
      self#unlink_segment current_segment ;
      Queue.iter self#unlink_segment segments ;
      Queue.clear segments ;
      List.iter
        (fun s ->
          match s.hls_oc with
            | None ->
                ()
            | Some segment ->
                self#close_out segment ;
                s.hls_oc <- None)
        streams

    method private playlist_name s = directory ^^ s.hls_name ^ ".m3u8"

    method private get_playlist_data =
      List.fold_left
        (fun ((_, _, l) as cur) el ->
          if List.length l < segments_per_playlist then
            (el.id, el.discontinuity_count, el :: l)
          else cur)
        (-1, -1, [])
        (List.rev (List.of_seq (Queue.to_seq segments)))

    method private write_playlists =
      let id, discontinuity_count, segments = self#get_playlist_data in
      List.iter
        (fun s ->
          if List.length segments > 0 then (
            let fname = self#playlist_name s in
            let oc = self#open_out fname in
            output_string oc "#EXTM3U\r\n" ;
            output_string oc
              (Printf.sprintf "#EXT-X-TARGETDURATION:%d\r\n"
                 (int_of_float (ceil segment_duration))) ;
            output_string oc (Printf.sprintf "#EXT-X-VERSION:%d\r\n" x_version) ;
            output_string oc (Printf.sprintf "#EXT-X-MEDIA-SEQUENCE:%d\r\n" id) ;
            output_string oc
              (Printf.sprintf "#EXT-X-DISCONTINUITY-SEQUENCE:%d\r\n"
                 discontinuity_count) ;
            List.iter
              (fun segment ->
                if segment.discontinuous then
                  output_string oc "#EXT-X-DISCONTINUITY\r\n" ;
                output_string oc
                  (Printf.sprintf "#EXTINF:%.03f,\r\n"
                     (Frame.seconds_of_master segment.len)) ;
                output_string oc
                  (self#segment_name ~relative:true ~segment s ^ "\r\n"))
              segments ;
            (* output_string oc "#EXT-X-ENDLIST\n"; *)
            self#close_out (fname, oc) ))
        streams ;
      let fname = directory ^^ playlist in
      let oc = self#open_out fname in
      output_string oc "#EXTM3U\r\n" ;
      output_string oc (Printf.sprintf "#EXT-X-VERSION:%d\r\n" x_version) ;
      List.iter
        (fun s ->
          let line =
            Printf.sprintf "#EXT-X-STREAM-INF:BANDWIDTH=%d,CODECS=%S\r\n"
              s.hls_bandwidth s.hls_codec
          in
          output_string oc line ;
          output_string oc (s.hls_name ^ ".m3u8\r\n"))
        streams ;
      self#close_out (fname, oc)

    method private cleanup_playlists =
      List.iter (fun s -> self#unlink (self#playlist_name s)) streams ;
      self#unlink (directory ^^ playlist)

    method output_start =
      if persist && Sys.file_exists persist_at then (
        try
          (self#log)#info "Resuming from saved state" ;
          self#read_state ;
          self#toggle_state `Resumed ;
          try Unix.unlink persist_at with _ -> ()
        with exn ->
          (self#log)#info "Failed to resume from saved state: %s"
            (Printexc.to_string exn) ;
          self#toggle_state `Start )
      else self#toggle_state `Start ;
      self#new_segment

    method output_stop =
      self#toggle_state `Stop ;
      let data = List.map (fun s -> s.hls_encoder.Encoder.stop ()) streams in
      self#send data ;
      if persist then (
        (self#log)#info "Saving state to %S.." persist_at ;
        self#push_current_segment ;
        self#write_state )
      else (self#cleanup_segments ; self#cleanup_playlists)

    method output_reset = self#toggle_state `Restart

    method private write_state =
      (self#log)#info "Reading state file at %S.." persist_at ;
      let fd = open_out_bin persist_at in
      Marshal.to_channel fd (current_segment, segments) [] ;
      close_out fd

    method private read_state =
      let fd = open_in_bin persist_at in
      let c, s = Marshal.from_channel fd in
      close_in fd ;
      current_segment <- c ;
      segments <- s

    method encode frame ofs len =
      if current_segment.len + len > segment_master_duration then (
        self#push_current_segment ; self#new_segment ) ;
      current_segment.len <- current_segment.len + len ;
      List.map (fun s -> s.hls_encoder.Encoder.encode frame ofs len) streams

    method send b = List.iter2 self#write_pipe streams b

    method insert_metadata m =
      if encode_metadata then
        List.iter (fun s -> s.hls_encoder.Encoder.insert_metadata m) streams
  end

let () =
  let kind = Lang.univ_t () in
  Lang.add_operator "output.file.hls" (hls_proto kind) ~active:true
    ~kind:(Lang.Unconstrained kind) ~category:Lang.Output
    ~descr:
      "Output the source stream to an HTTP live stream served from a local \
       directory." (fun p _ -> (new hls_output p :> Source.source))
