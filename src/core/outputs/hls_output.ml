(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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
  Liquidsoap_lang.Runtime.eval ~ignored:false ~ty:(Lang.univ_t ())
    {|fun (~position, ~extname, base) -> "#{base}_#{position}.#{extname}"|}

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
        (false, "position", Lang.int_t);
        (false, "extname", Lang.string_t);
        (false, "", Lang.string_t);
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
      ( "perm",
        Lang.int_t,
        Some (Lang.int 0o666),
        Some
          "Permission of the created files, up to umask. You can and should \
           write this number in octal notation: 0oXXX. The default value is \
           however displayed in decimal (0o666 = 6×8^2 + 4×8 + 4 = 412)." );
      ( "dir_perm",
        Lang.int_t,
        Some (Lang.int 0o777),
        Some
          "Permission of the directories if some have to be created, up to \
           umask. Although you can enter values in octal notation (0oXXX) they \
           will be displayed in decimal (for instance, 0o777 = 7×8^2 + 7×8 + 7 \
           = 511)." );
      ( "temp_dir",
        Lang.nullable_t Lang.string_t,
        Some Lang.null,
        Some
          "Temporary directory used for writing files. This should be in the \
           same partition or device as the final directory to guarantee atomic \
           file operations. Use an system-specific value if `null`." );
      ( "on_file_change",
        Lang.fun_t
          [(false, "state", Lang.string_t); (false, "", Lang.string_t)]
          Lang.unit_t,
        Some (Lang.val_cst_fun [("state", None); ("", None)] Lang.unit),
        Some
          "Callback executed when a file changes. `state` is one of: \
           `\"created\"`, `\"updated\"` or `\"deleted\"`, second argument is \
           file path. Typical use: sync file with a CDN" );
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
  ; close : unit >

type segment = {
  id : int;
  discontinuous : bool;
  current_discontinuity : int;
  filename : string;
  segment_extra_tags : string list;
  mutable init_filename : string option;
  mutable out_channel : atomic_out_channel option;
  mutable len : int;
}

let json_of_segment
    {
      id;
      discontinuous;
      current_discontinuity;
      filename;
      init_filename;
      segment_extra_tags;
      len;
    } =
  `Assoc
    [
      ("id", `Int id);
      ("discontinuous", `Bool discontinuous);
      ("current_discontinuity", `Int current_discontinuity);
      ("filename", `String filename);
      ( "init_filename",
        match init_filename with Some f -> `String f | None -> `Null );
      ("extra_tags", `Tuple (List.map (fun s -> `String s) segment_extra_tags));
      ("len", `Int len);
    ]

let segment_of_json = function
  | `Assoc
      [
        ("id", `Int id);
        ("discontinuous", `Bool discontinuous);
        ("current_discontinuity", `Int current_discontinuity);
        ("filename", `String filename);
        ("init_filename", init_filename);
        ("extra_tags", `Tuple segment_extra_tags);
        ("len", `Int len);
      ] ->
      let segment_extra_tags =
        List.map
          (function `String t -> t | _ -> raise Invalid_state)
          segment_extra_tags
      in
      let init_filename =
        match init_filename with
          | `String f -> Some f
          | `Null -> None
          | _ -> raise Invalid_state
      in
      {
        id;
        discontinuous;
        current_discontinuity;
        filename;
        init_filename;
        len;
        segment_extra_tags;
        out_channel = None;
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

(** A stream in the HLS (which typically contains many, with different qualities). *)
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
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply f [])
  in
  let on_file_change =
    let f = List.assoc "on_file_change" p in
    fun ~state filename ->
      ignore
        (Lang.apply f
           [
             ("state", Lang.string (string_of_file_state state));
             ("", Lang.string filename);
           ])
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
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
  let directory = Lang_string.home_unrelate (Lang.to_string directory_val) in
  let perms = Lang.to_int (List.assoc "perm" p) in
  let dir_perm = Lang.to_int (List.assoc "dir_perm" p) in
  let temp_dir =
    Lang.to_valued_option Lang.to_string (List.assoc "temp_dir" p)
  in
  let () =
    if (not (Sys.file_exists directory)) || not (Sys.is_directory directory)
    then (
      try Utils.mkdir ~perm:dir_perm directory
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
            Filename.concat directory filename
          else filename
        in
        let dir = Filename.dirname filename in
        (try Utils.mkdir ~perm:dir_perm dir
         with exn ->
           raise
             (Error.Invalid_value
                ( List.assoc "persist_at" p,
                  Printf.sprintf
                    "Error while creating directory %s for persisting state: %s"
                    (Lang_string.quote_string dir)
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
  let segment_name ~position ~extname sname =
    directory
    ^^ Lang.to_string
         (segment_name
            [
              ("position", Lang.int position);
              ("extname", Lang.string extname);
              ("", Lang.string sname);
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
        encoder_factory ~hls:true ~pos:fmt_val.Value.pos name
          Frame.Metadata.Export.empty
      in
      let bandwidth =
        lazy
          (try Lang.to_int (List.assoc "bandwidth" stream_info)
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
                              "Bandwidth for stream %S cannot be inferred from \
                               codec, please specify it with: \
                               `%%encoder(...).{bandwidth = <number>, ...}`"
                              name )))))
      in
      let codecs =
        lazy
          (try Lang.to_string (List.assoc "codecs" stream_info)
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
        lazy
          (try
             let w, h = Lang.to_product (List.assoc "video_size" stream_info) in
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
    lazy
      (if
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
  let source = Lang.assoc "" 3 p in
  let main_playlist_filename = Lang.to_string (List.assoc "playlist" p) in
  let main_playlist_filename = directory ^^ main_playlist_filename in
  let main_playlist_extra_tags =
    List.map
      (fun s -> String.trim (Lang.to_string s))
      (Lang.to_list (List.assoc "extra_tags" p))
  in
  let segments_per_playlist = Lang.to_int (List.assoc "segments" p) in
  let max_segments =
    segments_per_playlist + Lang.to_int (List.assoc "segments_overhead" p)
  in
  object (self)
    inherit
      Output.encoded
        ~infallible ~on_start ~on_stop ~autostart ~output_kind:"output.file"
          ~name:main_playlist_filename source

    (** Available segments *)
    val mutable segments = List.map (fun { name } -> (name, ref [])) streams

    val mutable streams = streams
    method streams = streams
    val mutable current_position = (0, 0)
    val mutable state : hls_state = `Idle

    method private toggle_state event =
      match (event, state) with
        | `Restart, _ | `Resumed, _ | `Start, `Stopped -> state <- `Restarted
        | `Stop, _ -> state <- `Stopped
        | `Start, _ -> state <- `Started
        | `Streaming, _ -> state <- `Streaming

    method private open_out filename =
      let state = if Sys.file_exists filename then `Updated else `Created in
      let mode = [Open_wronly; Open_creat; Open_trunc] in
      let tmp_file, oc =
        Filename.open_temp_file ?temp_dir ~mode ~perms "liq" "tmp"
      in
      set_binary_mode_out oc true;
      object
        method output_string = output_string oc
        method output_substring = output_substring oc

        method close =
          Stdlib.close_out_noerr oc;
          Fun.protect
            ~finally:(fun () -> try Sys.remove tmp_file with _ -> ())
            (fun () ->
              (try Unix.rename tmp_file filename
               with Unix.Unix_error (Unix.EXDEV, _, _) ->
                 self#log#important
                   "Rename failed! Directory for temporary files appears to be \
                    on a different file system. Please set it to the same one \
                    using `temp_dir` argument to guarantee atomic file \
                    operations!";
                 Utils.copy ~mode ~perms tmp_file filename;
                 Sys.remove tmp_file);
              on_file_change ~state filename)
      end

    method private unlink filename =
      self#log#debug "Cleaning up %s.." filename;
      on_file_change ~state:`Deleted filename;
      try Unix.unlink filename
      with Unix.Unix_error (e, _, _) ->
        self#log#important "Could not remove file %s: %s" filename
          (Unix.error_message e)

    method private close_segment s =
      ignore
        (Option.map
           (fun segment ->
             (Option.get segment.out_channel)#close;
             segment.out_channel <- None;
             let segments = List.assoc s.name segments in
             push_segment segment segments;
             if List.length !segments >= max_segments then (
               let segment = remove_segment segments in
               self#unlink segment.filename;
               ignore
                 (Option.map
                    (fun filename ->
                      if
                        Sys.file_exists filename
                        && not
                             (List.exists
                                (fun s ->
                                  s.init_filename = segment.init_filename)
                                !segments)
                      then self#unlink filename)
                    segment.init_filename)))
           s.current_segment);
      s.current_segment <- None;
      if state <> `Stopped then (
        self#write_playlist s;
        self#write_main_playlist)

    method private open_segment s =
      self#log#debug "Opening segment %d for stream %s." s.position s.name;
      let filename =
        segment_name ~position:s.position ~extname:s.extname s.name
      in
      let directory = Filename.dirname filename in
      let () =
        if (not (Sys.file_exists directory)) || not (Sys.is_directory directory)
        then (
          try Utils.mkdir ~perm:dir_perm directory
          with exn ->
            let bt = Printexc.get_raw_backtrace () in
            Lang.raise_as_runtime ~bt ~kind:"file" exn)
      in
      let out_channel = self#open_out filename in
      Strings.iter out_channel#output_substring (s.encoder.Encoder.header ());
      let discontinuous = state = `Restarted in
      state <- `Started;
      let segment =
        {
          id = s.position;
          discontinuous;
          current_discontinuity = s.discontinuity_count;
          len = 0;
          filename;
          segment_extra_tags = Atomic.get s.pending_extra_tags;
          init_filename =
            (match s.init_state with `Has_init f -> Some f | _ -> None);
          out_channel = Some out_channel;
        }
      in
      s.current_segment <- Some segment;
      s.position <- s.position + 1;
      Atomic.set s.pending_extra_tags [];
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
        ignore
          (Option.map out_channel#output_string
             (s.encoder.hls.insert_id3 ~frame_position ~sample_position m)));
      if discontinuous then s.discontinuity_count <- s.discontinuity_count + 1

    method private cleanup_streams =
      List.iter
        (fun (_, s) -> List.iter (fun s -> self#unlink s.filename) !s)
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
                 self#unlink segment.filename)
               s.current_segment);
          s.current_segment <- None)
        streams

    method private playlist_name s = directory ^^ s.name ^ ".m3u8"

    method private write_playlist s =
      let segments =
        List.fold_left
          (fun cur el ->
            if List.length cur < segments_per_playlist then el :: cur else cur)
          []
          (List.rev !(List.assoc s.name segments))
      in
      let discontinuity_sequence, media_sequence =
        match segments with
          | { current_discontinuity; id } :: _ -> (current_discontinuity, id - 1)
          | [] -> (0, 0)
      in
      let filename = self#playlist_name s in
      self#log#debug "Writing playlist %s.." s.name;
      let oc = self#open_out filename in
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
            (fun pos segment ->
              if segment.discontinuous then
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
                (Printf.sprintf "%s%s\r\n" prefix
                   (Filename.basename segment.filename)))
            segments)

    val mutable main_playlist_written = false

    method private write_main_playlist =
      match (main_playlist_writer, main_playlist_written) with
        | None, _ | Some _, true ->
            self#log#debug
              "`main_playlist_writer` is `null`: skipping main playlist"
        | Some main_playlist_writer, false ->
            let main_playlist =
              main_playlist_writer ~version:(Lazy.force x_version)
                ~extra_tags:main_playlist_extra_tags ~prefix streams
            in
            (match main_playlist with
              | None ->
                  self#log#debug
                    "main_playlist_writer returned `null`: skipping main \
                     playlist"
              | Some playlist ->
                  self#log#debug "Writing main playlist %s.."
                    main_playlist_filename;
                  let oc = self#open_out main_playlist_filename in
                  oc#output_string playlist;
                  oc#close);
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
           List.map (fun s -> (None, s.encoder.Encoder.stop ())) streams
         in
         self#send data
       with _ -> ());
      streams <- mk_streams ();
      match persist_at with
        | Some persist_at ->
            self#log#info "Saving state to %s.."
              (Lang_string.quote_string persist_at);
            List.iter (fun s -> self#close_segment s) streams;
            self#write_state persist_at
        | None ->
            self#cleanup_streams;
            self#cleanup_playlists

    method! reset = self#toggle_state `Restart

    method private write_state persist_at =
      self#log#info "Reading state file at %s.."
        (Lang_string.quote_string persist_at);
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
              segment_name ~position:init_position ~extname name
            in
            let oc = self#open_out init_filename in
            Fun.protect
              ~finally:(fun () -> oc#close)
              (fun () -> Strings.iter oc#output_substring data);
            segment.init_filename <- Some init_filename;
            s.init_state <- `Has_init init_filename
        | Some _ -> raise Encoder.Not_enough_data

    method private should_reopen ~segment ~len s =
      if s.id3_enabled && pending_metadata s.metadata then
        ( true,
          Printf.sprintf
            "Terminating current segment on stream %s to insert new metadata"
            s.name )
      else if Atomic.get s.pending_extra_tags <> [] then
        ( true,
          Printf.sprintf
            "Terminating current segment on stream %s to insert pending extra \
             tags"
            s.name )
      else if segment.len + len > segment_main_duration then
        ( true,
          Printf.sprintf
            "Terminating current segment on stream %s to make expected length"
            s.name )
      else (false, "")

    method encode frame ofs len =
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
                let init, encoded =
                  Encoder.(s.encoder.hls.init_encode frame ofs len)
                in
                self#process_init ~init ~segment s;
                (None, encoded)
              with Encoder.Not_enough_data -> (None, Strings.empty))
            else (
              let should_reopen, reason = self#should_reopen ~segment ~len s in
              if should_reopen then (
                match Encoder.(s.encoder.hls.split_encode frame ofs len) with
                  | `Ok (flushed, encoded) ->
                      self#log#info "%s" reason;
                      (Some flushed, encoded)
                  | `Nope encoded -> (None, encoded))
              else (None, Encoder.(s.encoder.encode frame ofs len)))
          in
          let segment = Option.get s.current_segment in
          segment.len <- segment.len + len;
          b)
        streams

    method private write_pipe s (flushed, data) =
      let { out_channel } = Option.get s.current_segment in
      ignore
        (Option.map
           (fun b ->
             let oc = Option.get out_channel in
             Strings.iter oc#output_substring b;
             self#close_segment s;
             self#open_segment s)
           flushed);
      let { out_channel } = Option.get s.current_segment in
      let oc = Option.get out_channel in
      Strings.iter oc#output_substring data

    method send b = List.iter2 self#write_pipe streams b

    method insert_metadata m =
      List.iter
        (fun s ->
          match s.metadata with
            | `Sent m' when Frame.Metadata.Export.equal m m' -> ()
            | _ -> s.metadata <- `Todo m)
        streams
  end

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
      ("insert_tag", Lang.fun_t [(false, "", Lang.string_t)] Lang.unit_t);
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
      ( "insert_tag",
        Lang.val_fun
          [("", "", None)]
          (fun p ->
            let tag = String.trim (Lang.to_string (List.assoc "" p)) in
            Atomic.set pending_extra_tags (tag :: Atomic.get pending_extra_tags);
            Lang.unit) );
    ]

let _ =
  let return_t = Lang.univ_t () in
  Lang.add_operator ~base:Pipe_output.output_file "hls" (hls_proto return_t)
    ~return_t ~category:`Output
    ~meth:
      ([
         ( "streams",
           ([], Lang.fun_t [] (Lang.list_t (stream_t return_t))),
           "Output streams",
           fun s ->
             Lang.val_fun [] (fun _ ->
                 Lang.list (List.map value_of_stream s#streams)) );
       ]
      @ Start_stop.meth ())
    ~descr:
      "Output the source stream to an HTTP live stream served from a local \
       directory."
    (fun p -> new hls_output p)
