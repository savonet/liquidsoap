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

(** HLS output. *)

exception Invalid_state

let default_id3_version = 3
let log = Log.make ["hls"; "output"]

let default_name =
  Lang.eval ~cache:false ~typecheck:false ~stdlib:`Disabled
    {|fun (metadata) -> "#{metadata.stream_name}_#{metadata.position}.#{metadata.extname}"|}

let hls_proto frame_t =
  let main_playlist_writer_t =
    Lang.fun_t
      [
        (false, "extra_tags", Lang.list_t Lang.string_t);
        (false, "prefix", Lang.string_t);
        (false, "version", Lang.int_t);
        ( false,
          "",
          Lang.list_t
            (Lang.optional_method_t
               (Lang.method_t Lang.string_t
                  [
                    ("bandwidth", ([], Lang.int_t), "Stream bandwidth");
                    ("codecs", ([], Lang.string_t), "Stream codecs");
                  ])
               [
                 ( "video_size",
                   ([], Lang.product_t Lang.int_t Lang.int_t),
                   "Stream video size" );
               ]) );
      ]
      (Lang.nullable_t Lang.string_t)
  in
  let segment_name_t =
    Lang.fun_t
      [
        ( false,
          "",
          Lang.record_t
            [
              ("position", Lang.int_t);
              ("extname", Lang.string_t);
              ("duration", Lang.float_t);
              ("ticks", Lang.int_t);
              ("stream_name", Lang.string_t);
            ] );
      ]
      Lang.string_t
  in
  let stream_info_t =
    Lang.product_t Lang.string_t
      (Lang.optional_method_t (Lang.format_t frame_t)
         [
           ("bandwidth", ([], Lang.int_t), "Bandwidth");
           ("codecs", ([], Lang.string_t), "Codec");
           ("extname", ([], Lang.string_t), "Filename extension");
           ( "id3",
             ([], Lang.bool_t),
             "Set to `false` to disable ID3 tags. Tags are enabled by default \
              whenever allowed." );
           ( "id3_version",
             ([], Lang.int_t),
             "Version for ID3 tag. Default version: "
             ^ string_of_int default_id3_version );
           ( "replay_id3",
             ([], Lang.bool_t),
             "Replay ID3 data on each segment to make sure new listeners \
              always start with fresh value. Enabled by default." );
           ("extra_tags", ([], Lang.list_t Lang.string_t), "Extra tags");
           ( "video_size",
             ([], Lang.product_t Lang.int_t Lang.int_t),
             "Video size" );
         ])
  in
  Output.proto
  @ [
      ( "playlist",
        Lang.string_t,
        Some (Lang.string "stream.m3u8"),
        Some "Playlist name (m3u8 extension is recommended)." );
      ( "extra_tags",
        Lang.list_t Lang.string_t,
        Some (Lang.list []),
        Some "Extra tags to insert into the main playlist." );
      ( "prefix",
        Lang.string_t,
        Some (Lang.string ""),
        Some "Prefix for each files in playlists." );
      ( "main_playlist_writer",
        Lang.nullable_t main_playlist_writer_t,
        Some Lang.null,
        Some
          "Main playlist writer. Main playlist writing is disabled when `null` \
           or when returning `null`." );
      ( "segment_duration",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Segment duration (in seconds)." );
      ( "segment_name",
        segment_name_t,
        Some default_name,
        Some
          "Segment name. Default: `fun (metadata) -> \
           \"#{metadata.stream_name}_#{metadata.position}.#{metadata.extname}\"`"
      );
      ( "segments_overhead",
        Lang.nullable_t Lang.int_t,
        Some (Lang.int 5),
        Some
          "Number of segments to keep after they have been featured in the \
           live playlist. Set to `null` to disable." );
      ( "segments",
        Lang.int_t,
        Some (Lang.int 10),
        Some "Number of segments per playlist." );
      ( "perm",
        Lang.int_t,
        Some (Lang.octal_int 0o666),
        Some "Permission of the created files, up to umask." );
      ( "dir_perm",
        Lang.int_t,
        Some (Lang.octal_int 0o777),
        Some
          "Permission of the directories if some have to be created, up to \
           umask." );
      ( "temp_dir",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Temporary directory used for writing files. This should be in the \
           same partition or device as the final directory to guarantee atomic \
           file operations. Use the same directory as the HLS files if `null`."
      );
      ( "persist_at",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Location of the configuration file used to restart the output. \
           Relative paths are assumed to be with regard to the directory for \
           generated file." );
      ( "strict_persist",
        Lang.bool_t,
        Some (Lang.bool false),
        Some "Fail if an invalid saved state exists." );
      ("", Lang.string_t, None, Some "Directory for generated files.");
      ( "",
        Lang.list_t stream_info_t,
        None,
        Some "List of specifications for each stream: (name, format)." );
      ("", Lang.source_t frame_t, None, None);
    ]

type atomic_out_channel =
  < output_string : string -> unit
  ; output_substring : string -> int -> int -> unit
  ; position : int
  ; truncate : int -> unit
  ; saved_filename : string option
  ; read : int -> int -> string
  ; close : unit >

type segment = {
  id : int;
  discontinuous : bool;
  current_discontinuity : int;
  segment_extra_tags : string list;
  mutable init_filename : string option;
  mutable filename : string option;
  mutable out_channel : atomic_out_channel option;
  (* Segment length in main ticks. *)
  mutable len : int;
  mutable last_segmentable_position : (int * int) option;
}

(* We used to encode optional entries with null but
   it's more future-proof to use undefined. These routines
   abstract it away. *)
let json_optional lbl f = function None -> [] | Some v -> [(lbl, f v)]

let parse_json_optional lbl f l =
  match List.assoc_opt lbl l with
    | Some `Null | None -> None
    | Some v -> Some (f v)

let parse_json lbl f l =
  match List.assoc_opt lbl l with Some v -> f v | None -> raise Invalid_state

let parse_json_int lbl l =
  parse_json lbl (function `Int i -> i | _ -> raise Invalid_state) l

let parse_json_bool lbl l =
  parse_json lbl (function `Bool b -> b | _ -> raise Invalid_state) l

let parse_json_string lbl l =
  parse_json lbl (function `String s -> s | _ -> raise Invalid_state) l

let json_of_segment
    {
      id;
      discontinuous;
      current_discontinuity;
      init_filename;
      filename;
      segment_extra_tags;
      len;
      last_segmentable_position;
    } =
  `Assoc
    ([
       ("id", `Int id);
       ("discontinuous", `Bool discontinuous);
       ("current_discontinuity", `Int current_discontinuity);
     ]
    @ json_optional "init_filename" (fun s -> `String s) init_filename
    @ json_optional "filename" (fun s -> `String s) filename
    @ [
        ("extra_tags", `Tuple (List.map (fun s -> `String s) segment_extra_tags));
        ("len", `Int len);
      ]
    @ json_optional "last_segmentable_position"
        (fun (len, offset) -> `Tuple [`Int len; `Int offset])
        last_segmentable_position)

let segment_of_json = function
  | `Assoc l ->
      let id = parse_json_int "id" l in
      let discontinuous = parse_json_bool "discontinuous" l in
      let current_discontinuity = parse_json_int "current_discontinuity" l in
      let segment_extra_tags =
        parse_json "extra_tags"
          (function
            | `Tuple l ->
                List.map
                  (function `String s -> s | _ -> raise Invalid_state)
                  l
            | _ -> raise Invalid_state)
          l
      in
      let len = parse_json_int "len" l in
      let init_filename =
        parse_json_optional "init_filename"
          (function `String s -> s | _ -> raise Invalid_state)
          l
      in
      let filename =
        parse_json_optional "filename"
          (function `String s -> s | _ -> raise Invalid_state)
          l
      in
      let last_segmentable_position =
        parse_json_optional "last_segmentable_position"
          (function
            | `Tuple [`Int len; `Int offset] -> (len, offset)
            | _ -> raise Invalid_state)
          l
      in
      {
        id;
        discontinuous;
        current_discontinuity;
        init_filename;
        len;
        segment_extra_tags;
        filename;
        out_channel = None;
        last_segmentable_position;
      }
  | _ -> raise Invalid_state

type segments = segment list ref

let push_segment segment segments = segments := !segments @ [segment]

let remove_segment segments =
  match !segments with
    | [] -> assert false
    | s :: l ->
        segments := l;
        s

type init_state = [ `Todo | `No_init | `Has_init of string ]

type metadata =
  [ `None | `Sent of Frame.Metadata.Export.t | `Todo of Frame.Metadata.Export.t ]

let pending_metadata = function `Todo _ -> true | _ -> false

(** A stream in the HLS (which typically contains many, with different
    qualities). *)
type stream = {
  name : string;
  format : Encoder.format;
  encoder : Encoder.encoder;
  video_size : (int * int) option Lazy.t;
  bandwidth : int Lazy.t;
  codecs : string Lazy.t;  (** codecs (see RFC 6381) *)
  extname : string;
  id3_enabled : bool;
  replay_id3 : bool;
  stream_extra_tags : string list;
  mutable pending_extra_tags : string list Atomic.t;
  mutable metadata : metadata;
  mutable init_state : init_state;
  mutable init_position : int;
  mutable position : int;
  mutable current_segment : segment option;
  mutable discontinuity_count : int;
}

type hls_state = [ `Idle | `Started | `Stopped | `Restarted | `Streaming ]

open Extralib

let ( ^^ ) = Filename.concat

type file_state = [ `Created | `Updated | `Deleted ]

let string_of_file_state = function
  | `Created -> "created"
  | `Updated -> "updated"
  | `Deleted -> "deleted"

class hls_output p =
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let register_telnet = Lang.to_bool (List.assoc "register_telnet" p) in
  let prefix = Lang.to_string (List.assoc "prefix" p) in
  let main_playlist_writer =
    Option.map
      (fun fn ~extra_tags ~version ~prefix streams ->
        let extra_tags = Lang.list (List.map Lang.string extra_tags) in
        let version = Lang.int version in
        let prefix = Lang.string prefix in
        let streams =
          Lang.list
            (List.map
               (fun stream ->
                 Lang.meth (Lang.string stream.name)
                   [
                     ("bandwidth", Lang.int (Lazy.force stream.bandwidth));
                     ("codecs", Lang.string (Lazy.force stream.codecs));
                     ( "video_size",
                       match Lazy.force stream.video_size with
                         | None -> Lang.null
                         | Some (w, h) -> Lang.product (Lang.int w) (Lang.int h)
                     );
                   ])
               streams)
        in
        Lang.to_valued_option Lang.to_string
          (Lang.apply fn
             [
               ("extra_tags", extra_tags);
               ("prefix", prefix);
               ("version", version);
               ("", streams);
             ]))
      (Lang.to_option (List.assoc "main_playlist_writer" p))
  in
  let directory_val = Lang.assoc "" 1 p in
  let hls_directory =
    Lang_string.home_unrelate (Lang.to_string directory_val)
  in
  let perms = Lang.to_int (List.assoc "perm" p) in
  let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
  let temp_dir =
    Lang.to_valued_option Lang.to_string (List.assoc "temp_dir" p)
  in
  let () =
    if
      (not (Sys.file_exists hls_directory))
      || not (Sys.is_directory hls_directory)
    then (
      try Utils.mkdir ~perm:dir_perm hls_directory
      with _ ->
        raise
          (Error.Invalid_value
             (directory_val, "Could not create or open output directory!")))
  in
  let persist_at =
    Option.map
      (fun filename ->
        let filename = Lang.to_string filename in
        let filename =
          if Filename.is_relative filename then
            Filename.concat hls_directory filename
          else filename
        in
        (try Utils.ensure_dir ~perm:dir_perm filename
         with exn ->
           raise
             (Error.Invalid_value
                ( List.assoc "persist_at" p,
                  Printf.sprintf
                    "Error while creating directory for persisting state at \
                     %s: %s"
                    (Lang_string.quote_string filename)
                    (Printexc.to_string exn) )));
        filename)
      (Lang.to_option (List.assoc "persist_at" p))
  in
  let strict_persist = Lang.to_bool (List.assoc "strict_persist" p) in
  (* better choice? *)
  let segment_duration = Lang.to_float (List.assoc "segment_duration" p) in
  let segment_ticks =
    Frame.main_of_seconds segment_duration / Lazy.force Frame.size
  in
  let segment_main_duration = segment_ticks * Lazy.force Frame.size in
  let segment_duration = Frame.seconds_of_main segment_main_duration in
  let segment_name = Lang.to_fun (List.assoc "segment_name" p) in
  let segment_name ~position ~extname ~duration ~ticks sname =
    Lang.to_string
      (segment_name
         [
           ( "",
             Lang.record
               [
                 ("position", Lang.int position);
                 ("extname", Lang.string extname);
                 ("duration", Lang.float duration);
                 ("ticks", Lang.int ticks);
                 ("stream_name", Lang.string sname);
               ] );
         ])
  in
  let streams =
    let streams = Lang.assoc "" 2 p in
    let l = Lang.to_list streams in
    if l = [] then
      raise
        (Error.Invalid_value (streams, "The list of streams cannot be empty"));
    l
  in
  let mk_streams, streams =
    let f s =
      let name, fmt_val = Lang.to_product s in
      let stream_info, fmt = Lang.split_meths fmt_val in
      let name = Lang.to_string name in
      let format = Lang.to_format fmt in
      let encoder_factory =
        try Encoder.get_factory format
        with Not_found ->
          raise (Error.Invalid_value (fmt_val, "Unsupported format"))
      in
      let encoder =
        encoder_factory ~hls:true ~pos:(Value.pos fmt_val) name
          Frame.Metadata.Export.empty
      in
      let bandwidth =
        Lazy.from_fun (fun () ->
            try Lang.to_int (List.assoc "bandwidth" stream_info)
            with Not_found -> (
              match Encoder.(encoder.hls.bitrate ()) with
                | Some b -> b + (b / 10)
                | None -> (
                    try Encoder.bitrate format
                    with Not_found ->
                      raise
                        (Error.Invalid_value
                           ( fmt,
                             Printf.sprintf
                               "Bandwidth for stream %S cannot be inferred \
                                from codec, please specify it with: \
                                `%%encoder(...).{bandwidth = <number>, ...}`"
                               name )))))
      in
      let codecs =
        Lazy.from_fun (fun () ->
            try Lang.to_string (List.assoc "codecs" stream_info)
            with Not_found -> (
              match Encoder.(encoder.hls.codec_attrs ()) with
                | Some attrs -> attrs
                | None -> (
                    try Encoder.iso_base_file_media_file_format format
                    with Not_found ->
                      raise
                        (Error.Invalid_value
                           ( fmt,
                             Printf.sprintf
                               "Stream info for stream %S cannot be inferred \
                                from codec, please specify it with: \
                                `%%encoder(...).{codecs = \"...\", ...}`"
                               name )))))
      in
      let extname =
        try Lang.to_string (List.assoc "extname" stream_info)
        with Not_found -> (
          try Encoder.extension format
          with Not_found ->
            raise
              (Error.Invalid_value
                 ( fmt,
                   Printf.sprintf
                     "File extension for stream %S cannot be inferred from \
                      codec, please specify it with: `%%encoder(...).{extname \
                      = \"...\", ...}`"
                     name )))
      in
      let extname = if extname = "mp4" then "m4s" else extname in
      let video_size =
        Lazy.from_fun (fun () ->
            try
              let w, h =
                Lang.to_product (List.assoc "video_size" stream_info)
              in
              Some (Lang.to_int w, Lang.to_int h)
            with Not_found -> (
              match Encoder.(encoder.hls.video_size ()) with
                | Some s -> Some s
                | None -> Encoder.video_size format))
      in
      let id3_enabled =
        match Lang.to_bool (List.assoc "id3" stream_info) with
          | id3_enabled ->
              let id3_version =
                try Some (Lang.to_int (List.assoc "id3_version" stream_info))
                with Not_found -> None
              in
              encoder.hls.init ~id3_enabled ?id3_version ()
          | exception Not_found -> encoder.hls.init ()
      in
      let replay_id3 =
        match Lang.to_bool (List.assoc "replay_id3" stream_info) with
          | b -> b
          | exception Not_found -> true
      in
      let stream_extra_tags =
        match
          List.map
            (fun s -> String.trim (Lang.to_string s))
            (Lang.to_list (List.assoc "extra_tags" stream_info))
        with
          | l -> l
          | exception Not_found -> []
      in
      {
        name;
        format;
        encoder;
        bandwidth;
        codecs;
        video_size;
        extname;
        id3_enabled;
        replay_id3;
        stream_extra_tags;
        pending_extra_tags = Atomic.make [];
        metadata = `None;
        init_state = `Todo;
        init_position = 0;
        position = 1;
        current_segment = None;
        discontinuity_count = 0;
      }
    in
    let mk_streams () = List.map f streams in
    (mk_streams, mk_streams ())
  in
  let x_version =
    Lazy.from_fun (fun () ->
        if
          List.find_opt
            (fun s ->
              match s.current_segment with
                | Some { init_filename = Some _ } -> true
                | _ -> false)
            streams
          <> None
        then 7
        else 3)
  in
  let source_val = Lang.assoc "" 3 p in
  let source = Lang.to_source source_val in
  let main_playlist_filename = Lang.to_string (List.assoc "playlist" p) in
  let main_playlist_extra_tags =
    List.map
      (fun s -> String.trim (Lang.to_string s))
      (Lang.to_list (List.assoc "extra_tags" p))
  in
  let segments_per_playlist = Lang.to_int (List.assoc "segments" p) in
  let segments_overhead =
    Lang.to_valued_option Lang.to_int (List.assoc "segments_overhead" p)
  in
  let max_segments =
    Option.map
      (fun segments_overhead -> segments_per_playlist + segments_overhead)
      segments_overhead
  in
  object (self)
    inherit
      [(int * Strings.t option * Strings.t) list] Output.encoded
        ~infallible ~register_telnet ~autostart ~export_cover_metadata:false
          ~output_kind:"output.file" ~name:main_playlist_filename source_val

    (** Available segments *)
    val mutable segments = List.map (fun { name } -> (name, ref [])) streams

    val mutable streams = streams
    method streams = streams
    val mutable current_position = (0, 0)
    val mutable state : hls_state = `Idle
    method self_sync = source#self_sync
    val mutable on_file_change : (state:file_state -> string -> unit) list = []
    method on_file_change fn = on_file_change <- fn :: on_file_change

    method private toggle_state event =
      match (event, state) with
        | `Restart, _ | `Resumed, _ | `Start, `Stopped -> state <- `Restarted
        | `Stop, _ -> state <- `Stopped
        | `Start, _ -> state <- `Started
        | `Streaming, _ -> state <- `Streaming

    method private open_out filename =
      let temp_dir = Option.value ~default:hls_directory temp_dir in
      let tmp_file = Filename.temp_file ~temp_dir "liq" "tmp" in
      Unix.chmod tmp_file perms;
      let fd =
        Unix.openfile tmp_file
          [Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC]
          perms
      in
      object
        method output_string s = Tutils.write_all fd (Bytes.unsafe_of_string s)

        method output_substring s ofs len =
          Tutils.write_all fd (Bytes.sub (Bytes.unsafe_of_string s) ofs len)

        method position = Unix.lseek fd 0 Unix.SEEK_CUR
        method truncate = Unix.ftruncate fd

        method read ofs len =
          Unix.fsync fd;
          assert (ofs = Unix.lseek fd ofs Unix.SEEK_SET);
          let b = Bytes.create len in
          let rec f n =
            if n < len then (
              let r = Unix.read fd b n (len - n) in
              if r <> 0 then f (n + r) else n)
            else n
          in
          Bytes.sub_string b 0 (f 0)

        val mutable saved_filename = None
        method saved_filename = saved_filename

        method close =
          (try Unix.close fd with _ -> ());
          Fun.protect
            ~finally:(fun () -> try Sys.remove tmp_file with _ -> ())
            (fun () ->
              let fname = Filename.concat hls_directory (filename ()) in
              Utils.ensure_dir ~perm:dir_perm fname;
              saved_filename <- Some fname;
              let state =
                if Sys.file_exists fname then `Updated else `Created
              in
              (try Unix.rename tmp_file fname
               with Unix.Unix_error (Unix.EXDEV, _, _) ->
                 self#log#important
                   "Rename failed! Directory for temporary files appears to be \
                    on a different file system. Please set it to the same one \
                    using `temp_dir` argument to guarantee atomic file \
                    operations!";
                 Utils.copy
                   ~mode:[Open_creat; Open_trunc; Open_binary]
                   ~perms tmp_file fname;
                 Sys.remove tmp_file);
              List.iter (fun fn -> fn ~state fname) on_file_change)
      end

    method private unlink filename =
      self#log#debug "Cleaning up %s.." filename;
      List.iter (fun fn -> fn ~state:`Deleted filename) on_file_change;
      try Unix.unlink filename
      with Unix.Unix_error (e, _, _) ->
        self#log#important "Could not remove file %s: %s" filename
          (Unix.error_message e)

    method private unlink_segment =
      function { filename = Some filename } -> self#unlink filename | _ -> ()

    method private close_segment =
      function
      | { current_segment = Some ({ out_channel = Some oc } as segment) } as s
        ->
          oc#close;
          segment.filename <- oc#saved_filename;
          segment.out_channel <- None;
          let segments = List.assoc s.name segments in
          push_segment segment segments;
          if
            match max_segments with
              | None -> false
              | Some max_segments -> List.length !segments >= max_segments
          then (
            let segment = remove_segment segments in
            self#unlink_segment segment;
            match segment.init_filename with
              | None -> ()
              | Some filename ->
                  if
                    Sys.file_exists filename
                    && not
                         (List.exists
                            (fun s -> s.init_filename = segment.init_filename)
                            !segments)
                  then self#unlink filename);
          s.current_segment <- None;
          if state <> `Stopped then (
            self#write_playlist s;
            self#write_main_playlist)
      | _ -> ()

    method private open_segment s =
      self#log#debug "Opening segment %d for stream %s." s.position s.name;
      let discontinuous, current_discontinuity =
        if state = `Restarted then (true, s.discontinuity_count + 1)
        else (false, s.discontinuity_count)
      in
      state <- `Started;
      let segment_extra_tags = Atomic.exchange s.pending_extra_tags [] in
      let segment =
        {
          id = s.position;
          discontinuous;
          current_discontinuity;
          len = 0;
          segment_extra_tags;
          init_filename =
            (match s.init_state with `Has_init f -> Some f | _ -> None);
          filename = None;
          out_channel = None;
          last_segmentable_position = None;
        }
      in
      let { position; extname } = s in
      let filename () =
        let ticks = segment.len in
        let duration = Frame.seconds_of_main ticks in
        segment_name ~position ~extname ~duration ~ticks s.name
      in
      let out_channel = self#open_out filename in
      Strings.iter out_channel#output_substring (s.encoder.Encoder.header ());
      segment.out_channel <- Some out_channel;
      s.current_segment <- Some segment;
      s.discontinuity_count <- current_discontinuity;
      s.position <- s.position + 1;
      if s.id3_enabled then (
        let m =
          match s.metadata with
            | `Todo m ->
                s.metadata <- `Sent m;
                Frame.Metadata.Export.to_list m
            | `Sent m when s.replay_id3 -> Frame.Metadata.Export.to_list m
            | `Sent _ | `None -> []
        in
        let frame_position, sample_position = current_position in
        match s.encoder.hls.insert_id3 ~frame_position ~sample_position m with
          | None -> ()
          | Some s -> out_channel#output_string s)

    method reopen_segment ~position:(len, offset) =
      function
      | {
          current_segment =
            Some
              ({ len = current_len; out_channel = Some oc } as current_segment);
        } as s ->
          let rem = oc#read offset (oc#position - offset) in
          current_segment.len <- len;
          oc#truncate offset;
          self#close_segment s;
          self#open_segment s;
          let segment = Option.get s.current_segment in
          let oc = Option.get segment.out_channel in
          oc#output_string rem;
          segment.len <- current_len - len
      | _ -> assert false

    method private cleanup_streams =
      List.iter
        (fun (_, s) -> List.iter (fun s -> self#unlink_segment s) !s)
        segments;
      List.iter
        (fun s ->
          ignore
            (Option.map
               (fun segment ->
                 (try (Option.get segment.out_channel)#close with _ -> ());
                 ignore
                   (Option.map
                      (fun filename ->
                        if Sys.file_exists filename then self#unlink filename)
                      segment.init_filename);
                 self#unlink_segment segment)
               s.current_segment);
          s.current_segment <- None)
        streams

    method private playlist_name s = s.name ^ ".m3u8"

    method private write_playlist s =
      let segments =
        List.filter_map
          (function
            | { filename = Some fname } as s -> Some (fname, s) | _ -> None)
          (List.rev !(List.assoc s.name segments))
      in
      let segments =
        List.fold_left
          (fun cur el ->
            if List.length cur < segments_per_playlist then el :: cur else cur)
          [] segments
      in
      let discontinuity_sequence, media_sequence =
        match segments with
          | (_, { current_discontinuity; id }) :: _ ->
              (current_discontinuity, id - 1)
          | [] -> (0, 0)
      in
      let filename = self#playlist_name s in
      self#log#debug "Writing playlist %s.." s.name;
      let oc = self#open_out (fun () -> filename) in
      Fun.protect
        ~finally:(fun () -> oc#close)
        (fun () ->
          oc#output_string "#EXTM3U\r\n";
          oc#output_string
            (Printf.sprintf "#EXT-X-TARGETDURATION:%d\r\n"
               (int_of_float (ceil segment_duration)));
          oc#output_string
            (Printf.sprintf "#EXT-X-VERSION:%d\r\n" (Lazy.force x_version));
          oc#output_string
            (Printf.sprintf "#EXT-X-MEDIA-SEQUENCE:%d\r\n" media_sequence);
          oc#output_string
            (Printf.sprintf "#EXT-X-DISCONTINUITY-SEQUENCE:%d\r\n"
               discontinuity_sequence);
          List.iter
            (fun tag ->
              oc#output_string tag;
              oc#output_string "\r\n")
            s.stream_extra_tags;
          List.iteri
            (fun pos (filename, segment) ->
              if 0 < pos && segment.discontinuous then
                oc#output_string "#EXT-X-DISCONTINUITY\r\n";
              if pos = 0 || segment.discontinuous then (
                match segment.init_filename with
                  | Some filename ->
                      let filename =
                        Printf.sprintf "%s%s" prefix
                          (Filename.basename filename)
                      in
                      oc#output_string
                        (Printf.sprintf "#EXT-X-MAP:URI=%s\r\n"
                           (Lang_string.quote_string filename))
                  | _ -> ());
              oc#output_string
                (Printf.sprintf "#EXTINF:%.03f,\r\n"
                   (Frame.seconds_of_main segment.len));
              List.iter
                (fun tag ->
                  oc#output_string tag;
                  oc#output_string "\r\n")
                segment.segment_extra_tags;
              oc#output_string
                (Printf.sprintf "%s%s\r\n" prefix (Filename.basename filename)))
            segments)

    val mutable main_playlist_written = false

    method private write_main_playlist =
      (match (main_playlist_writer, main_playlist_written) with
        | _, true -> ()
        | None, false ->
            self#log#debug
              "`main_playlist_writer` is `null`: skipping main playlist"
        | Some main_playlist_writer, false -> (
            let main_playlist =
              main_playlist_writer ~version:(Lazy.force x_version)
                ~extra_tags:main_playlist_extra_tags ~prefix streams
            in
            match main_playlist with
              | None ->
                  self#log#debug
                    "main_playlist_writer returned `null`: skipping main \
                     playlist"
              | Some playlist ->
                  self#log#debug "Writing main playlist %s.."
                    main_playlist_filename;
                  let oc = self#open_out (fun () -> main_playlist_filename) in
                  oc#output_string playlist;
                  oc#close));
      main_playlist_written <- true

    method private cleanup_playlists =
      List.iter (fun s -> self#unlink (self#playlist_name s)) streams;
      self#unlink main_playlist_filename;
      main_playlist_written <- false

    method start =
      (match persist_at with
        | Some persist_at when Sys.file_exists persist_at -> (
            try
              self#log#info "Resuming from saved state";
              self#read_state persist_at;
              self#toggle_state `Resumed;
              try Unix.unlink persist_at with _ -> ()
            with exn when not strict_persist ->
              self#log#info "Failed to resume from saved state: %s"
                (Printexc.to_string exn);
              self#toggle_state `Start)
        | _ -> self#toggle_state `Start);
      List.iter self#open_segment streams;
      self#toggle_state `Streaming

    method stop =
      self#toggle_state `Stop;
      (try
         let data =
           List.map (fun s -> (0, None, s.encoder.Encoder.stop ())) streams
         in
         self#send data
       with _ -> ());
      (match persist_at with
        | Some persist_at ->
            self#log#info "Saving state to %s.."
              (Lang_string.quote_string persist_at);
            List.iter (fun s -> self#close_segment s) streams;
            self#write_state persist_at
        | None ->
            self#cleanup_streams;
            self#cleanup_playlists);
      streams <- mk_streams ()

    method! reset = self#toggle_state `Restart

    method private write_state persist_at =
      let fd = open_out_bin persist_at in
      let streams =
        `Tuple
          (List.map
             (fun { name; position; discontinuity_count; pending_extra_tags } ->
               `Assoc
                 [
                   ("name", `String name);
                   ("position", `Int position);
                   ( "pending_extra_tags",
                     `Tuple
                       (List.map
                          (fun s -> `String s)
                          (Atomic.get pending_extra_tags)) );
                   ("discontinuity_count", `Int discontinuity_count);
                 ])
             streams)
      in
      let segments =
        `Assoc
          (List.map
             (fun (s, l) -> (s, `Tuple (List.map json_of_segment !l)))
             segments)
      in
      output_string fd
        (Json.to_string ~compact:false ~json5:false
           (`Assoc [("streams", streams); ("segments", segments)]));
      close_out fd

    method private read_state persist_at =
      let saved_streams, saved_segments =
        match Json.from_string (Utils.read_all persist_at) with
          | `Assoc [("streams", streams); ("segments", segments)] ->
              (streams, segments)
          | _ -> raise Invalid_state
      in
      let saved_streams =
        List.map
          (function
            | `Assoc
                [
                  ("name", `String name);
                  ("position", `Int position);
                  ("pending_extra_tags", `Tuple pending_extra_tags);
                  ("discontinuity_count", `Int discontinuity_count);
                ] ->
                let pending_extra_tags =
                  List.map
                    (function `String s -> s | _ -> raise Invalid_state)
                    pending_extra_tags
                in
                (name, position, pending_extra_tags, discontinuity_count)
            | _ -> raise Invalid_state)
          (match saved_streams with `Tuple l -> l | _ -> raise Invalid_state)
      in
      let saved_segments =
        match saved_segments with
          | `Assoc l ->
              List.map
                (function
                  | s, `Tuple segments ->
                      (s, ref (List.map segment_of_json segments))
                  | _ -> raise Invalid_state)
                l
          | _ -> raise Invalid_state
      in
      List.iter2
        (fun stream (name, pos, pending_extra_tags, discontinuity_count) ->
          assert (name = stream.name);
          Atomic.set stream.pending_extra_tags pending_extra_tags;
          stream.discontinuity_count <- discontinuity_count;
          stream.init_position <- pos;
          stream.position <- pos + 1)
        streams saved_streams;
      segments <- saved_segments

    method private process_init ~init ~segment
        ({ extname; name; init_position } as s) =
      match init with
        | None -> s.init_state <- `No_init
        | Some data when not (Strings.is_empty data) ->
            let init_filename =
              segment_name ~position:init_position ~extname ~duration:0.
                ~ticks:0 name
            in
            let oc = self#open_out (fun () -> init_filename) in
            Fun.protect
              ~finally:(fun () -> oc#close)
              (fun () -> Strings.iter oc#output_substring data);
            segment.init_filename <- Some init_filename;
            s.init_state <- `Has_init init_filename
        | Some _ -> raise Encoder.Not_enough_data

    method private should_reopen ~segment ~len s =
      if segment.len + len > segment_main_duration then
        ( true,
          Printf.sprintf
            "Terminating current segment %d on stream %s to make expected \
             length"
            segment.id s.name,
          true )
      else if s.id3_enabled && pending_metadata s.metadata then
        ( true,
          Printf.sprintf
            "Terminating current segment %d on stream %s to insert new metadata"
            segment.id s.name,
          false )
      else if Atomic.get s.pending_extra_tags <> [] then
        ( true,
          Printf.sprintf
            "Terminating current segment %d on stream %s to insert pending \
             extra tags"
            segment.id s.name,
          false )
      else (false, "", false)

    method encode frame =
      let len = Frame.position frame in
      let frame_pos, samples_pos = current_position in
      let frame_size = Lazy.force Frame.size in
      let samples_pos = samples_pos + len in
      current_position <-
        (frame_pos + (samples_pos / frame_size), samples_pos mod frame_size);
      List.map
        (fun s ->
          let segment = Option.get s.current_segment in
          let b =
            if s.init_state = `Todo then (
              try
                let init, encoded = Encoder.(s.encoder.hls.init_encode frame) in
                self#process_init ~init ~segment s;
                (len, None, encoded)
              with Encoder.Not_enough_data -> (len, None, Strings.empty))
            else (
              match Encoder.(s.encoder.hls.split_encode frame) with
                | `Ok (flushed, encoded) -> (len, Some flushed, encoded)
                | `Nope encoded -> (len, None, encoded))
          in
          let segment = Option.get s.current_segment in
          segment.len <- segment.len + len;
          b)
        streams

    method private write_pipe s (len, flushed, data) =
      let ({ out_channel } as segment) = Option.get s.current_segment in
      let oc = Option.get out_channel in
      (match flushed with
        | None -> ()
        | Some b ->
            Strings.iter oc#output_substring b;
            segment.last_segmentable_position <- Some (segment.len, oc#position));
      (match
         (self#should_reopen ~segment ~len s, segment.last_segmentable_position)
       with
        | (false, _, _), _ | (true, _, false), None -> ()
        | (true, reason, _), position ->
            let position =
              match position with
                | None ->
                    self#log#important
                      "Splitting segment without a new keyframe! You might \
                       want to adjust your encoder's parameters to increase \
                       the keyframe frequency!";
                    (segment.len, oc#position)
                | Some p -> p
            in
            self#log#info "%s" reason;
            self#reopen_segment ~position s);
      let { out_channel } = Option.get s.current_segment in
      let oc = Option.get out_channel in
      Strings.iter oc#output_substring data

    method send b = List.iter2 self#write_pipe streams b

    method encode_metadata m =
      List.iter
        (fun s ->
          match s.metadata with
            | `Sent m' when Frame.Metadata.Export.equal m m' -> ()
            | _ -> s.metadata <- `Todo m)
        streams
  end

let insert_tag_t = Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t

let insert_tag pending_extra_tags_list =
  Lang.val_fun
    [("", "", None)]
    (fun p ->
      let tag = String.trim (Lang.to_string (List.assoc "" p)) in
      List.iter
        (fun pending_extra_tags ->
          let rec append () =
            let tags = Atomic.get pending_extra_tags in
            if
              not (Atomic.compare_and_set pending_extra_tags tags (tag :: tags))
            then append ()
          in
          append ())
        pending_extra_tags_list;
      Lang.unit)

let stream_t kind =
  Lang.record_t
    [
      ("name", Lang.string_t);
      ("encoder", Lang.format_t kind);
      ( "video_size",
        Lang.nullable_t
          (Lang.record_t [("width", Lang.int_t); ("height", Lang.int_t)]) );
      ("bandwidth", Lang.int_t);
      ("codecs", Lang.string_t);
      ("extname", Lang.string_t);
      ("id3_enabled", Lang.bool_t);
      ("replay_id3", Lang.bool_t);
      ("extra_tags", Lang.list_t Lang.string_t);
      ("discontinuity_count", Lang.int_t);
      ("insert_tag", insert_tag_t);
    ]

let value_of_stream
    {
      name;
      format;
      video_size;
      bandwidth;
      codecs;
      extname;
      id3_enabled;
      replay_id3;
      stream_extra_tags;
      discontinuity_count;
      pending_extra_tags;
    } =
  Lang.record
    [
      ("name", Lang.string name);
      ("encoder", Lang_encoder.L.format format);
      ( "video_size",
        match Lazy.force video_size with
          | None -> Lang.null
          | Some (w, h) ->
              Lang.record [("width", Lang.int w); ("height", Lang.int h)] );
      ("bandwidth", Lang.int (Lazy.force bandwidth));
      ("codecs", Lang.string (Lazy.force codecs));
      ("extname", Lang.string extname);
      ("id3_enabled", Lang.bool id3_enabled);
      ("replay_id3", Lang.bool replay_id3);
      ("extra_tags", Lang.list (List.map Lang.string stream_extra_tags));
      ("discontinuity_count", Lang.int discontinuity_count);
      ("insert_tag", insert_tag [pending_extra_tags]);
    ]

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Pipe_output.output_file "hls" (hls_proto return_t)
    ~return_t ~category:`Output
    ~meth:
      (Lang.
         [
           {
             name = "insert_tag";
             scheme = ([], insert_tag_t);
             descr = "Insert the same tag into all the streams";
             value =
               (fun s ->
                 insert_tag
                   (List.map
                      (fun { pending_extra_tags } -> pending_extra_tags)
                      s#streams));
           };
           {
             name = "streams";
             scheme = ([], Lang.fun_t [] (Lang.list_t (stream_t return_t)));
             descr = "Output streams";
             value =
               (fun s ->
                 Lang.val_fun [] (fun _ ->
                     Lang.list (List.map value_of_stream s#streams)));
           };
         ]
      @ Start_stop.meth ())
    ~callbacks:
      ([
         {
           Lang_source.name = "on_file_change";
           params = [];
           descr =
             "when a file changes. `state` is one of: `\"created\"`, \
              `\"updated\"` or `\"deleted\"`, `path` is the full file path. \
              Typical use: sync file with a CDN";
           default_synchronous = false;
           register_deprecated_argument = false;
           arg_t =
             [
               ( false,
                 "",
                 Lang.record_t
                   [("state", Lang.string_t); ("path", Lang.string_t)] );
             ];
           register =
             (fun ~params:_ s on_file_change ->
               let on_file_change ~state path =
                 on_file_change
                   [
                     ( "",
                       Lang.record
                         [
                           ("state", Lang.string (string_of_file_state state));
                           ("path", Lang.string path);
                         ] );
                   ]
               in
               s#on_file_change on_file_change);
         };
       ]
      @ Start_stop.callbacks ~label:"output")
    ~descr:
      "Output the source stream to an HTTP live stream served from a local \
       directory."
    (fun p -> new hls_output p)
