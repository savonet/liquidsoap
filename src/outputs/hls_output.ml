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

let hls_proto kind =
  let segment_name_t = Lang.fun_t [
    false, "duration", Lang.float_t;
    false, "position", Lang.int_t;
    false, "extname", Lang.string_t;
    false, "", Lang.string_t
  ] Lang.string_t in
  let default_name = Lang.val_fun [
    "duration", "duration", Lang.float_t, None;
    "position", "position", Lang.int_t, None;
    "extname", "extname", Lang.string_t, None;
    "", "", Lang.string_t, None
  ] ~ret_t:Lang.string_t (fun p _ ->
    let position = Lang.to_int (List.assoc "position" p) in
    let extname = Lang.to_string (List.assoc "extname" p) in
    let sname = Lang.to_string (List.assoc "" p) in
    Lang.string (Printf.sprintf "%s_%d.%s" sname position extname))
  in
  (Output.proto @ [
     "playlist",
     Lang.string_t,
     Some (Lang.string "stream.m3u8"),
     Some "Playlist name (m3u8 extension is recommended).";

     "segment_duration",
     Lang.float_t,
     Some (Lang.float 10.),
     Some "Segment duration (in seconds).";

     "segment_name",
     segment_name_t,
     Some default_name,
     Some "Segment name. \
           Default: `fun (~duration=_,~position,~extname,stream_name) -> \"#{stream_name}_#{position}.#{extname}\"`";

     "segments",
     Lang.int_t,
     Some (Lang.int 15),
     Some "Number of segments to keep on disk.";

     "segments_per_playlist",
     Lang.int_t,
     Some (Lang.int 10),
     Some "Number of segments per playlist.";

     "perm",
     Lang.int_t,
     Some (Lang.int 0o644),
     Some "Permission of the created files, up to umask. \
           You can and should write this number in octal notation: 0oXXX. \
           The default value is however displayed in decimal \
           (0o666 = 6*8^2 + 4*8 + 4 = 412)." ;

     "on_file_change",
     Lang.fun_t [false,"state",Lang.string_t;
                 false,"",Lang.string_t] Lang.unit_t,
      Some (Lang.val_cst_fun ["state",Lang.string_t,None;
                              "",Lang.string_t,None] Lang.unit),
      Some "Callback executed when a file changes. `state` is one of: \
            `\"opened\"`, `\"closed\"` or `\"deleted\"`, second argument is \
            file path. Typical use: upload files to a CDN when done writting (`\"close\"` \
            state and remove when `\"deleted\"`.";

     "",
     Lang.string_t,
     None,
     Some "Directory for generated files." ;

     "",
     Lang.list_t (Lang.product_t Lang.string_t (Lang.format_t kind)),
     None,
     Some "List of specifications for each stream: (name, format).";

     "", Lang.source_t kind, None, None
  ])

type segment =
  {
     id:            int;
     discontinuous: bool;
     discontinuity: int;
     mutable len:   int
  }

(** A stream in the HLS (which typically contains many, with different qualities). *)
type hls_stream_desc =
  {
    hls_name : string; (** name of the stream *)
    hls_format : Encoder.format;
    hls_encoder : Encoder.encoder;
    hls_bandwidth : int option;
    hls_codec : string option; (** codec (see RFC 6381) *)
    mutable hls_oc : (string*out_channel) option; (** currently encoded file *)
  }

open Extralib

let (^^) = Filename.concat

type file_state = [`Opened|`Closed|`Deleted]

let string_of_file_state = function
  | `Opened -> "opened"
  | `Closed -> "closed"
  | `Deleted -> "deleted"

class hls_output p =
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_file_change =
    let f = List.assoc "on_file_change" p in
    fun ~state fname ->
      ignore (Lang.apply ~t:Lang.unit_t f ["state",Lang.string (string_of_file_state state);
                                           "",Lang.string fname])
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let directory = Lang.to_string (Lang.assoc "" 1 p) in
  let () =
    if not (Sys.file_exists directory) || not (Sys.is_directory directory) then
      raise (Lang_errors.Invalid_value (Lang.assoc "" 1 p, "The target directory does not exist"))
  in
  let streams =
    let streams = Lang.assoc "" 2 p in
    let l = Lang.to_list streams in
    if l = [] then raise (Lang_errors.Invalid_value (streams, "The list of streams cannot be empty"));
    l
  in
  let streams =
    let f s =
      let name, fmt = Lang.to_product s in
      let hls_name = Lang.to_string name in
      let hls_format = Lang.to_format fmt in
      let hls_bandwidth =
        try
          Some (Encoder.bitrate hls_format)
        with Not_found -> None
      in
      let hls_encoder_factory =
        try Encoder.get_factory hls_format
        with Not_found -> raise (Lang_errors.Invalid_value (fmt, "Unsupported format"))
      in
      let hls_encoder =
        hls_encoder_factory hls_name Meta_format.empty_metadata
      in
      let hls_codec =
        try
          Some (Encoder.iso_base_file_media_file_format hls_format)
        with Not_found ->
          log#important "Unknown ISO Base Media File Format, none will be output in the playlist.";
          None
      in
      {
        hls_name;
        hls_format;
        hls_encoder;
        hls_bandwidth;
        hls_codec;
        hls_oc = None;
      }
    in
    let streams = List.map f streams in
    streams
  in
  let source = Lang.assoc "" 3 p in
  let playlist = Lang.to_string (List.assoc "playlist" p) in
  let name = playlist in (* better choice? *)
  let segment_duration =
    Lang.to_float (List.assoc "segment_duration" p)
  in
  let segment_ticks =
    Frame.master_of_seconds segment_duration /
      Lazy.force Frame.size
  in
  let segment_duration =
    Frame.seconds_of_master (segment_ticks * Lazy.force Frame.size)
  in
  let segment_name =
    Lang.to_fun ~t:Lang.string_t (List.assoc "segment_name" p)
  in
  let segment_name ~duration ~position ~extname sname =
    Lang.to_string (segment_name [
      "duration",Lang.float duration;
      "position",Lang.int position;
      "extname",Lang.string extname;
      "",Lang.string sname
    ])
  in
  let max_segments = Lang.to_int (List.assoc "segments" p) in
  let segments_per_playlist =
    Lang.to_int (List.assoc "segments_per_playlist" p)
  in
  let file_perm = Lang.to_int (List.assoc "perm" p) in
  let kind = Encoder.kind_of_format (List.hd streams).hls_format in
  object (self)
    inherit
      Output.encoded
        ~infallible ~on_start ~on_stop ~autostart
        ~output_kind:"output.file" ~name
        ~content_kind:kind source as output

    (** Current segment ID *)
    val mutable current_segment =
      {id=(-1);discontinuous=false;discontinuity=0;len=0}

    (** Available segments *)
    val mutable segments = Queue.create ()

    (** Opening date for current segment. *)
    val mutable open_tick = 0
    val mutable current_metadata = None

    val mutable state = `Idle

    method private toggle_state event =
      match event, state with
        | `Restart, _
        | `Start,  `Stopped -> state <- `Restarted
        | `Stop,    _       -> state <- `Stopped
        | `Start,   _       -> state <- `Started
        | `Streaming, _     -> state <- `Streaming

    method private segment_name ?(relative=false) ~segment stream =
      let duration =
        Frame.seconds_of_master segment.len
      in
      let fname =
        segment_name ~position:segment.id ~duration
                     ~extname:(Encoder.extension stream.hls_format)
                     stream.hls_name
      in
      (if relative then "" else directory) ^^ fname

    method private open_out fname =
      let mode = [Open_wronly; Open_creat; Open_trunc] in
      let oc = open_out_gen mode file_perm fname in
      set_binary_mode_out oc true;
      on_file_change ~state:`Opened fname;
      oc

    method private unlink fname =
      on_file_change ~state:`Deleted fname;
      try
        Unix.unlink fname
      with Unix.Unix_error (_, _, msg) ->
        self#log#important "Could not remove file %s: %s" fname msg

    method private unlink_segment segment =
      self#log#debug "Cleaning up segment %d.." segment.id ;
      List.iter (fun s ->
        self#unlink (self#segment_name ~segment s)) streams

    method private close_out (fname, oc) =
      close_out oc;
      on_file_change ~state:`Closed fname

    method private close_segment s =
      match s.hls_oc with 
        | None -> ()
        | Some v ->
            self#close_out v;
            s.hls_oc <- None

    method private open_segment s =
      let meta = match current_metadata with
        | Some m -> m
        | None -> Meta_format.empty_metadata
      in
      s.hls_encoder.Encoder.insert_metadata meta; 
      let fname = self#segment_name ~segment:current_segment s in
      let oc = self#open_out fname in
      s.hls_oc <- Some (fname, oc);
      match s.hls_encoder.Encoder.header with
        | Some s -> output_string oc s;
        | None -> ()

    method private new_segment =
      if current_segment.id <> -1 then
        Queue.push current_segment segments;
      let discontinuous = state = `Restarted in
      let discontinuity =
        if current_segment.discontinuous then
          current_segment.discontinuity + 1
        else
          current_segment.discontinuity
      in
      self#toggle_state `Streaming;
      let id = current_segment.id + 1 in
      let len = 0 in
      current_segment <- {id;discontinuous;discontinuity;len};
      open_tick <- self#current_tick;
      self#log#debug "Opening segment %d." current_segment.id ;
      List.iter (fun s ->
        self#close_segment s;
        self#open_segment s) streams;
      let old_segment =
        if Queue.length segments >= max_segments then
          Some (Queue.take segments)
        else
          None
      in
      self#write_playlists;
      match old_segment with
        | Some s -> self#unlink_segment s
        | None -> ()  

    method private current_tick =
      if Source.Clock_variables.is_known self#clock then
        (Source.Clock_variables.get self#clock)#get_tick
      else
        0

    method private write_pipe s b =
      let _, oc = Utils.get_some s.hls_oc in
      output_string oc b

    method private cleanup_segments =
      self#unlink_segment current_segment;
      Queue.iter self#unlink_segment segments;
      Queue.clear segments;
      List.iter (fun s ->
          self#close_out (Utils.get_some s.hls_oc);
          s.hls_oc <- None
        ) streams

    method private playlist_name s =
      directory^^s.hls_name^".m3u8"

    method private get_playlist_data =
      List.fold_left (fun ((_,_,l) as cur) el ->
        if List.length l < segments_per_playlist then
          el.id, el.discontinuity, (el::l)
        else
          cur) (-1,-1,[]) (List.rev (List.of_seq (Queue.to_seq segments)))

    method private write_playlists =
      let id, discontinuity, segments =
        self#get_playlist_data
      in
      List.iter (fun s ->
          if List.length segments > 0 then
           begin
            let fname = self#playlist_name s in
            let oc = self#open_out fname in
            output_string oc "#EXTM3U\r\n";
            output_string oc (Printf.sprintf "#EXT-X-TARGETDURATION:%d\r\n" (int_of_float (ceil segment_duration)));
            output_string oc "#EXT-X-VERSION:3\r\n";
            output_string oc (Printf.sprintf "#EXT-X-MEDIA-SEQUENCE:%d\r\n" id);
            output_string oc (Printf.sprintf "#EXT-X-DISCONTINUITY-SEQUENCE:%d\r\n" discontinuity);
            List.iter (fun segment ->
                if segment.discontinuous then
                  output_string oc "#EXT-X-DISCONTINUITY\r\n";
                output_string oc (Printf.sprintf "#EXTINF:%.03f,\r\n" (Frame.seconds_of_master segment.len));
                output_string oc ((self#segment_name ~relative:true ~segment s) ^ "\r\n")
              ) segments;
            (* output_string oc "#EXT-X-ENDLIST\n"; *)
            self#close_out (fname, oc);
           end
        ) streams;
      let fname = directory^^playlist in
      let oc = self#open_out fname in
      output_string oc "#EXTM3U\n";
      List.iter (fun s ->
          let line =
            let bandwidth =
              match s.hls_bandwidth with
                | Some b -> Printf.sprintf "AVERAGE-BANDWIDTH=%d,BANDWIDTH=%d" b b
                | None -> ""
            in
            let codecs =
              match s.hls_codec with
              | Some codec -> Printf.sprintf "CODECS=\"%s\"" codec
              | None -> ""
            in
            Printf.sprintf
              "#EXT-X-STREAM-INF:%s%s%s\n"
              bandwidth (if bandwidth = "" then "" else ",") codecs
          in
          output_string oc line;
          output_string oc (s.hls_name^".m3u8\n")
        ) streams;
      self#close_out (fname, oc)

    method private cleanup_playlists =
      List.iter (fun s ->
        self#unlink (self#playlist_name s)) streams;
      self#unlink (directory^^playlist)

    method output_start =
      self#toggle_state `Start;
      self#new_segment

    method output_stop =
      self#toggle_state `Stop

    method output_reset =
      self#toggle_state `Restart

    method sleep =
      self#cleanup_segments;
      self#cleanup_playlists;
      output#sleep

    method encode frame ofs len =
      current_segment.len <- current_segment.len + len;
      List.map (fun s ->
          s.hls_encoder.Encoder.encode frame ofs len
        ) streams

    method send b =
      List.iter2 self#write_pipe streams b;
      if self#current_tick - open_tick > segment_ticks then
        self#new_segment

    method insert_metadata m =
      List.iter (fun s ->
        s.hls_encoder.Encoder.insert_metadata m) streams
  end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "output.file.hls" (hls_proto kind) ~active:true
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Output
    ~descr:"Output the source stream to an HTTP live stream served from a local directory."
    (fun p _ -> ((new hls_output p):>Source.source))
