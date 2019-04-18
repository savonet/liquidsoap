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

let hls_proto kind =
  (Output.proto @ [
     "playlist",
     Lang.string_t,
     Some (Lang.string "stream.m3u8"),
     Some "Playlist name (m3u8 extension is recommended).";

     "segment",
     Lang.float_t,
     Some (Lang.float 10.),
     Some "Segment duration (in seconds).";

     "segments",
     Lang.int_t,
     Some (Lang.int 10),
     Some "Number of segments to keep.";

     "perm",
     Lang.int_t,
     Some (Lang.int 0o644),
     Some "Permission of the created files, up to umask. \
           You can and should write this number in octal notation: 0oXXX. \
           The default value is however displayed in decimal \
           (0o666 = 6*8^2 + 4*8 + 4 = 412)." ;

     "",
     Lang.string_t,
     None,
     Some "Directory for generated files." ;

     "",
     Lang.list_t (Lang.uple_t [Lang.string_t; Lang.format_t kind; Lang.int_t; Lang.string_t]),
     None,
     Some "List of specifications for each stream: name, format, bandwidth (bits per second), optional parameters (unused for now).";

     "", Lang.source_t kind, None, None
  ])

(** A stream in the HLS (which typically contains many, with different qualities). *)
type hls_stream_desc =
  {
    hls_name : string; (** name of the stream *)
    hls_format : Encoder.format;
    hls_encoder_factory : Encoder.factory;
    mutable hls_encoder : Encoder.encoder option;
    hls_bandwidth : int;
    hls_codec : string; (** codec (see RFC 6381) *)
    mutable hls_oc : out_channel option; (** currently encoded file *)
  }

open Extralib

(* TODO: can we share more with other classes? *)
class hls_output p =
  let on_start =
    let f = List.assoc "on_start" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let on_stop =
    let f = List.assoc "on_stop" p in
    fun () -> ignore (Lang.apply ~t:Lang.unit_t f [])
  in
  let autostart = Lang.to_bool (List.assoc "start" p) in
  let infallible = not (Lang.to_bool (List.assoc "fallible" p)) in
  let directory = Lang.to_string (Lang.assoc "" 1 p) in
  let () = if not (Sys.file_exists directory) || not (Sys.is_directory directory) then raise (Lang.Invalid_value (Lang.assoc "" 1 p, "The target directory does not exist")) in
  let streams =
    let v = Lang.assoc "" 2 p in
    let streams = Lang.to_list v in
    let f s =
      match Lang.to_uple s with
      | [n; f; b; _] ->
         let hls_format = Lang.to_format f in
         let hls_encoder_factory =
           try Encoder.get_factory hls_format
           with Not_found -> raise (Lang.Invalid_value (f, "Unsupported format"))
         in
         let hls_codec = Encoder.rfc6381 hls_format in (* TODO: from the format, see RFC 6381 *)
         {
           hls_name = Lang.to_string n;
           hls_format;
           hls_encoder_factory;
           hls_encoder = None;
           hls_bandwidth = Lang.to_int b;
           hls_codec;
           hls_oc = None;
         }
      | _ -> assert false
    in
    let streams = List.map f streams in
    if streams = [] then raise (Lang.Invalid_value (v, "The list of streams cannot be empty"));
    streams
  in
  let source = Lang.assoc "" 3 p in
  let playlist = Lang.to_string (List.assoc "playlist" p) in
  let name = playlist in (* better choice? *)
  let segment_duration = Lang.to_float (List.assoc "segment" p) in
  let segment_nb = Lang.to_int (List.assoc "segments" p) in
  let file_perm = Lang.to_int (List.assoc "perm" p) in
  let kind = Encoder.kind_of_format (List.hd streams).hls_format in
object (self)
  val mutable current_filename = None

  inherit
    Output.encoded
      ~infallible ~on_start ~on_stop ~autostart
      ~output_kind:"output.file" ~name
      ~content_kind:kind source

  (** Current segment *)
  val mutable segment = -1

  (** Available segments *)
  val mutable segments = []

  (** Opening date for current segment. *)
  val mutable open_date = 0.
  val mutable current_metadata = None

  method segment_name ?(relative=false) ?(segment=segment) stream =
    Printf.sprintf "%s%s_%d.%s" (if relative then "" else directory^"/") stream.hls_name segment (Encoder.extension stream.hls_format)

  method is_open =
    (List.hd streams).hls_oc <> None

  method open_pipes =
    List.iter (fun s ->
        segment <- segment + 1;
        let mode = [Open_wronly; Open_creat; Open_trunc] in
        let oc = open_out_gen mode file_perm (self#segment_name s) in
        set_binary_mode_out oc true;
        s.hls_oc <- Some oc;
      ) streams;
    segments <- segment :: segments;
    while List.length segments > segment_nb do
      (* TODO: be more efficient *)
      let l = List.rev segments in
      let rm = List.hd l in
      segments <- List.rev (List.tl l);
      List.iter (fun s -> Unix.unlink (self#segment_name ~segment:rm s)) streams
    done;
    open_date <- Unix.gettimeofday ()

  method write_pipe s b =
    let oc = Utils.get_some s.hls_oc in
    output_string oc b

  method close_pipes =
    List.iter (fun s ->
        close_out (Utils.get_some s.hls_oc);
        s.hls_oc <- None
      ) streams

  method write_playlists =
    List.iter (fun s ->
        let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] file_perm (directory^"/"^s.hls_name^".m3u8") in
        output_string oc "#EXTM3U\n";
        output_string oc (Printf.sprintf "#EXT-X-TARGETDURATION:%d\n" (int_of_float (segment_duration +. 1.)));
        output_string oc (Printf.sprintf "#EXT-X-MEDIA-SEQUENCE:%d\n" (List.last segments));
        output_string oc "#EXT-X-PLAYLIST-TYPE:VOD\n";
        List.iter (fun segment ->
            output_string oc (Printf.sprintf "#EXTINF:%d,\n" (int_of_float (segment_duration +. 0.5)));
            output_string oc ((self#segment_name ~relative:true ~segment s) ^ "\n")
          ) (List.rev segments);
        (* output_string oc "#EXT-X-ENDLIST\n"; *)
        close_out oc
      ) streams;
    let oc = open_out_gen [Open_wronly; Open_creat; Open_trunc] file_perm (directory^"/"^playlist) in
    output_string oc "#EXTM3U\n";
    List.iter (fun s ->
        let line =
          Printf.sprintf
            "#EXT-X-STREAM-INF:AVERAGE-BANDWIDTH=%d,BANDWIDTH=%d,CODECS=\"%s\"\n"
            s.hls_bandwidth s.hls_bandwidth s.hls_codec
        in
        output_string oc line;
        output_string oc (s.hls_name^".m3u8\n")
      ) streams;
    close_out oc

  method output_start =
    List.iter (fun s ->
        assert (not (self#is_open && s.hls_encoder = None));
        let enc = s.hls_encoder_factory self#id in
        let meta = match current_metadata with
          | Some m -> m
          | None -> Meta_format.empty_metadata
        in
        s.hls_encoder <- Some (enc meta)
      ) streams;
    self#open_pipes;
    self#write_playlists

  method output_stop =
    let flush =
      List.map (fun s ->
          let b = (Utils.get_some s.hls_encoder).Encoder.stop () in
          s.hls_encoder <- None;
          b
        ) streams
    in
    self#send flush;
    self#close_pipes

  method output_reset = ()

  val mutable reopening = false

  method encode frame ofs len =
    List.map (fun s ->
        let enc = Utils.get_some s.hls_encoder in
        enc.Encoder.encode frame ofs len
      ) streams

  method send b =
    if not self#is_open then self#open_pipes;
    List.iter2 self#write_pipe streams b;
    if not reopening && Unix.gettimeofday () > segment_duration +. open_date then
      begin
        self#log#f 5 "New segment..." ;
        (* #output_stop can trigger #send, the [reopening] flag avoids loops *)
        reopening <- true;
        self#output_stop;
        self#output_start;
        reopening <- false;
      end

  method insert_metadata m =
    List.iter (fun s -> (Utils.get_some s.hls_encoder).Encoder.insert_metadata m) streams
end

let () =
  let kind = Lang.univ_t 1 in
  Lang.add_operator "output.hls" (hls_proto kind) ~active:true
    ~kind:(Lang.Unconstrained kind)
    ~category:Lang.Output
    ~descr:"Output the source stream to an HTTP live stream."
    (fun p _ -> ((new hls_output p):>Source.source))
