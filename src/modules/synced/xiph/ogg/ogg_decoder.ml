(*
 * Copyright 2007-2011 Savonet team
 *
 * This file is part of ocaml-ogg.
 *
 * ocaml-ogg is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-ogg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-ogg; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with
 * a publicly distributed version of the Library to produce an executable file
 * containing portions of the Library, and distribute that executable file
 * under terms of your choice, without any of the additional requirements
 * listed in clause 6 of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either
 * the unmodified Library as distributed by INRIA, or a modified version of
 * the Library that is distributed under the conditions defined in clause 3
 * of the GNU Library General Public License. This exception does not however
 * invalidate any other reasons why the executable file might be covered by
 * the GNU Library General Public License.
 *
 *)

(** Ogg stream demuxer *)

type metadata = string * (string * string) list

type ('a, 'b) decoder = {
  name : string;
  info : unit -> 'a * metadata;
  decode : ('b -> unit) -> unit;
  restart : fill:(unit -> unit) -> Ogg.Stream.stream -> unit;
  samples_of_granulepos : Int64.t -> Int64.t;
}

type audio_info = { channels : int; sample_rate : int }
type audio_data = float array array

type audio_ba_data =
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t array

type video_plane =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

(** Only supported for now: plannar YUV formats. *)
type video_format =
  | Yuvj_420
    (* Planar YCbCr 4:2:0. Each component is an uint8_t,
     * luma and chroma values are full range (0x00 .. 0xff) *)
  | Yuvj_422
    (* Planar YCbCr 4:2:2. Each component is an uint8_t,
     * luma and chroma values are full range (0x00 .. 0xff) *)
  | Yuvj_444

(* Planar YCbCr 4:4:4. Each component is an uint8_t,
 * luma and chroma values are full range (0x00 .. 0xff) *)

type video_info = {
  fps_numerator : int;
  fps_denominator : int;
  width : int;  (** Width of the Y' luminance plane *)
  height : int;  (** Height of the luminance plane *)
}

type video_data = {
  format : video_format;
  frame_width : int;
  frame_height : int;
  y_stride : int;  (** Length, in bytes, per line *)
  uv_stride : int;  (** Length, in bytes, per line *)
  y : video_plane;  (** luminance data *)
  u : video_plane;  (** Cb data *)
  v : video_plane;  (** Cr data *)
}

type decoders =
  | Video of (video_info, video_data) decoder
  | Audio of (audio_info, audio_data) decoder
  | Audio_ba of (audio_info, audio_ba_data) decoder
  | Audio_both of
      (audio_info, audio_data) decoder * (audio_info, audio_ba_data) decoder
  | Unknown

type callbacks = {
  read : bytes -> int -> int -> int;
  seek : (int -> int) option;
  tell : (unit -> int) option;
}

type index_element = {
  index_bytes : int;
  samples : Int64.t;
  total_samples : Int64.t;
}

type stream = {
  mutable os : Ogg.Stream.stream;
  mutable position : float;
  index : (Int64.t, index_element) Hashtbl.t;
  mutable read_samples : Int64.t;
  dec : decoders;
}

type t = {
  sync : Ogg.Sync.t;
  callbacks : callbacks;
  mutable started : bool;
  mutable last_p : int option;
  log : string -> unit;
  streams : (nativeint, stream) Hashtbl.t;
  finished_streams : (nativeint, stream) Hashtbl.t;
}

type track =
  | Audio_track of (string * nativeint)
  | Video_track of (string * nativeint)

exception Internal of (Ogg.Page.t * int option)
exception Exit of nativeint * Ogg.Stream.stream * decoders
exception Track of (bool * nativeint * stream)
exception Invalid_stream
exception Not_available

(* This exception has a different semantics than [Ogg.End_of_stream].
 * [Ogg.End_of_stream] is raised when end of data has been reached,
 * while this exception is raised when end of a logical stream has
 * been reached.. *)
exception End_of_stream

type register_decoder =
  (Ogg.Stream.packet -> bool)
  * (fill:(unit -> unit) -> Ogg.Stream.stream -> decoders)

let get_some x = match x with Some x -> x | None -> assert false
let ogg_decoders = Hashtbl.create 1
let log dec = Printf.ksprintf dec.log

(* End of stream is declared only when
 * all logical stream have ended (dec.streams = 0)
 * _and_ all their data has been consumed (dec.finished_streams = 0) *)
let eos dec =
  dec.started
  && Hashtbl.length dec.streams = 0
  && Hashtbl.length dec.finished_streams = 0

let granuleconv dec granulepos cur =
  try
    let ret =
      match dec with
        | Audio_ba d -> d.samples_of_granulepos granulepos
        | Audio_both (d, _) -> d.samples_of_granulepos granulepos
        | Audio d -> d.samples_of_granulepos granulepos
        | Video d -> d.samples_of_granulepos granulepos
        | Unknown -> assert false
    in
    if ret > Int64.zero then ret else cur
  with _ -> cur

let feed_page ~position decoder page =
  let serial = Ogg.Page.serialno page in
  try
    let stream = Hashtbl.find decoder.streams serial in
    if stream.dec <> Unknown then begin
      Ogg.Stream.put_page stream.os page;
      let granulepos = Ogg.Page.granulepos page in
      let total_samples =
        granuleconv stream.dec granulepos stream.read_samples
      in
      if total_samples > stream.read_samples then begin
        begin match position with
          | Some p ->
              if not (Hashtbl.mem stream.index granulepos) then
                Hashtbl.add stream.index granulepos
                  {
                    index_bytes = p;
                    samples = Int64.sub total_samples stream.read_samples;
                    total_samples = stream.read_samples;
                  }
          | None -> ()
        end;
        stream.read_samples <- total_samples
      end
    end;
    if Ogg.Page.eos page then begin
      log decoder "Reached last page of logical stream %nx" serial;
      Hashtbl.remove decoder.streams serial;
      if stream.dec <> Unknown then
        (* Moving finished stream to decoder.finished_streams *)
        Hashtbl.add decoder.finished_streams serial stream
    end
  with Not_found ->
    log decoder "Couldn't find a decoder for page in stream %nx" serial;
    raise Invalid_stream

let get_page decoder =
  if eos decoder then raise End_of_stream;
  let position =
    match decoder.callbacks.tell with None -> None | Some f -> Some (f ())
  in
  let page = Ogg.Sync.read decoder.sync in
  match decoder.callbacks.tell with
    | Some f ->
        if Some (f ()) = position then (decoder.last_p, page)
        else begin
          let pos = decoder.last_p in
          decoder.last_p <- position;
          (pos, page)
        end
    | _ -> (None, page)

let feed decoder =
  let position, page = get_page decoder in
  feed_page ~position decoder page

let test dec page =
  let serial = Ogg.Page.serialno page in
  log dec "Found a ogg logical stream, serial: %nx" serial;
  let os = Ogg.Stream.create ~serial () in
  Ogg.Stream.put_page os page;
  (* Get first packet *)
  let packet = Ogg.Stream.peek_packet os in
  try
    Hashtbl.iter
      (fun format (check, decode) ->
        log dec "Trying ogg/%s format" format;
        if check packet then (
          log dec "ogg/%s format detected for stream %nx" format serial;
          raise (Exit (serial, os, decode ~fill:(fun () -> feed dec) os)))
        else ())
      ogg_decoders;
    log dec "Couldn't find a decoder for ogg logical stream with serial %nx"
      serial;
    raise (Exit (serial, os, Unknown))
  with Exit (s, o, d) -> (s, o, d)

(** This should be called only * when we are near the end of * a stream... *)
let abort dec =
  dec.started <- true;
  begin try
    while Hashtbl.length dec.streams > 0 do
      feed dec
    done
  with _ -> Hashtbl.clear dec.streams
  end;
  Hashtbl.clear dec.finished_streams

let parse dec =
  assert (not (eos dec));
  let rec parse () =
    try
      (* Get First page *)
      let position, page = get_page dec in
      (* Check whether this is a b_o_s *)
      if not (Ogg.Page.bos page) then raise (Internal (page, position));
      let serial, os, decoder = test dec page in
      (* Should not happen *)
      if Hashtbl.mem dec.streams serial then raise Invalid_stream;
      let stream =
        {
          os;
          position = 0.;
          read_samples = Int64.zero;
          index = Hashtbl.create 10;
          dec = decoder;
        }
      in
      Hashtbl.add dec.streams serial stream;
      parse ()
    with Internal (p, position) -> feed_page ~position dec p
  in
  parse ();
  dec.started <- true;
  dec

let init ?(log = fun _ -> ()) c =
  let sync = Ogg.Sync.create c.read in
  let streams = Hashtbl.create 2 in
  let finished_streams = Hashtbl.create 2 in
  let pos = match c.tell with None -> None | Some f -> Some (f ()) in
  parse
    {
      sync;
      started = false;
      log;
      streams;
      callbacks = c;
      last_p = pos;
      finished_streams;
    }

let unix_callbacks fd =
  {
    read = Unix.read fd;
    tell = Some (fun () -> Unix.lseek fd 0 Unix.SEEK_CUR);
    seek = Some (fun len -> Unix.lseek fd len Unix.SEEK_SET);
  }

let init_from_fd ?log fd = init ?log (unix_callbacks fd)

let init_from_file ?log filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
  (init_from_fd ?log fd, fd)

let get_ogg_sync dec = dec.sync

let reset dec =
  if Hashtbl.length dec.streams > 0 || Hashtbl.length dec.finished_streams > 0
  then log dec "Resetting a stream that has not ended!";
  Hashtbl.clear dec.streams;
  Hashtbl.clear dec.finished_streams;
  dec.started <- false;
  ignore (parse dec)

let fold_tracks dec f x =
  let x = Hashtbl.fold f dec.streams x in
  Hashtbl.fold f dec.finished_streams x

let get_track dec dtype =
  let test ended id stream =
    match (stream.dec, dtype) with
      | Audio_ba _, Audio_track (_, x) when x = id ->
          raise (Track (ended, id, stream))
      | Audio_both _, Audio_track (_, x) when x = id ->
          raise (Track (ended, id, stream))
      | Audio _, Audio_track (_, x) when x = id ->
          raise (Track (ended, id, stream))
      | Video _, Video_track (_, x) when x = id ->
          raise (Track (ended, id, stream))
      | _ -> ()
  in
  try
    (* First check active streams *)
    Hashtbl.iter (test false) dec.streams;
    (* Now check finished streams *)
    Hashtbl.iter (test true) dec.finished_streams;
    raise Not_found
  with Track t -> t

let get_tracks dec =
  let f id stream l =
    match stream.dec with
      | Audio_ba d -> Audio_track (d.name, id) :: l
      | Audio_both (d, _) -> Audio_track (d.name, id) :: l
      | Audio d -> Audio_track (d.name, id) :: l
      | Video d -> Video_track (d.name, id) :: l
      | Unknown -> l
  in
  fold_tracks dec f []

type standard_tracks = {
  mutable audio_track : track option;
  mutable video_track : track option;
}

let drop_track dec dtype =
  (* Remove all track of this type *)
  let get_tracks id s l =
    match (s.dec, dtype) with
      | Audio_ba _, Audio_track (_, x) when x = id -> (id, s) :: l
      | Audio_both _, Audio_track (_, x) when x = id -> (id, s) :: l
      | Audio _, Audio_track (_, x) when x = id -> (id, s) :: l
      | Video _, Video_track (_, x) when x = id -> (id, s) :: l
      | _ -> l
  in
  let tracks = fold_tracks dec get_tracks [] in
  let stype =
    match dtype with Audio_track _ -> "audio" | Video_track _ -> "video"
  in
  let f (a, x) =
    log dec "Dropping %s track with serial %nx." stype a;
    Hashtbl.replace dec.streams a
      {
        os = x.os;
        index = x.index;
        read_samples = x.read_samples;
        position = x.position;
        dec = Unknown;
      }
  in
  List.iter f tracks

let get_standard_tracks ?tracks dec =
  let f id stream (a_t, v_t, l) =
    match stream.dec with
      | Audio_ba d when a_t = None -> (Some (Audio_track (d.name, id)), v_t, l)
      | Audio_ba d -> (a_t, v_t, Audio_track (d.name, id) :: l)
      | Audio_both (d, _) when a_t = None ->
          (Some (Audio_track (d.name, id)), v_t, l)
      | Audio_both (d, _) -> (a_t, v_t, Audio_track (d.name, id) :: l)
      | Audio d when a_t = None -> (Some (Audio_track (d.name, id)), v_t, l)
      | Audio d -> (a_t, v_t, Audio_track (d.name, id) :: l)
      | Video d when v_t = None -> (a_t, Some (Video_track (d.name, id)), l)
      | Video d -> (a_t, v_t, Video_track (d.name, id) :: l)
      | _ -> (a_t, v_t, l)
  in
  let a_t, v_t, drop = fold_tracks dec f (None, None, []) in
  List.iter (drop_track dec) drop;
  match tracks with
    | None -> { audio_track = a_t; video_track = v_t }
    | Some x ->
        x.audio_track <- a_t;
        x.video_track <- v_t;
        x

let update_standard_tracks dec tracks = ignore (get_standard_tracks ~tracks dec)
let get_standard_tracks dec = get_standard_tracks dec

let rec sample_rate_priv d dec =
  try
    match d with
      | Audio_ba d -> ((fst (d.info ())).sample_rate, 1)
      | Audio_both (d, _) -> ((fst (d.info ())).sample_rate, 1)
      | Audio d -> ((fst (d.info ())).sample_rate, 1)
      | Video d ->
          ((fst (d.info ())).fps_numerator, (fst (d.info ())).fps_denominator)
      | _ -> assert false
  with Ogg.Not_enough_data ->
    feed dec;
    sample_rate_priv d dec

let sample_rate dec dtype =
  let _, _, stream = get_track dec dtype in
  sample_rate_priv stream.dec dec

let get_track_position dec dtype =
  let _, _, stream = get_track dec dtype in
  stream.position

let get_position dec =
  if Hashtbl.length dec.streams = 0 && Hashtbl.length dec.finished_streams = 0
  then raise Not_available;
  let f _ stream pos =
    match stream.dec with
      | Audio_ba _ | Audio_both _ | Audio _ | Video _ -> min stream.position pos
      | _ -> pos
  in
  fold_tracks dec f max_float

let can_seek dec = dec.callbacks.seek <> None && dec.callbacks.tell <> None

type sync_point = {
  sync_stream : stream;
  sync_id : nativeint;
  sync_rate : float;
  mutable sync_seen : bool;
  mutable sync_granulepos : Int64.t;
  mutable sync_skip_samples : int;
  mutable sync_bytes : int;
}

(* Function to seek at a given point. *)
let sync_seek dec pos =
  Ogg.Sync.reset dec.sync;
  let seek = get_some dec.callbacks.seek in
  ignore (seek pos);
  Ogg.Sync.seek dec.sync

exception Position of (Int64.t * index_element)

let find_seek_pos dec time sync_point =
  let samples = Int64.of_float (time *. sync_point.sync_rate) in
  while sync_point.sync_stream.read_samples <= samples do
    feed dec
  done;
  let f granulepos index_element =
    if
      index_element.total_samples <= samples
      && Int64.add index_element.total_samples index_element.samples >= samples
    then raise (Position (granulepos, index_element))
  in
  let granulepos, index_element =
    try
      Hashtbl.iter f sync_point.sync_stream.index;
      raise Not_found
    with Position x -> x
  in
  let skip_samples = Int64.sub samples index_element.total_samples in
  sync_point.sync_stream.read_samples <- index_element.total_samples;
  sync_point.sync_granulepos <- granulepos;
  sync_point.sync_skip_samples <- Int64.to_int skip_samples;
  sync_point.sync_bytes <- index_element.index_bytes;
  sync_point.sync_stream.position <-
    Int64.to_float
      (Int64.add sync_point.sync_stream.read_samples
         (Int64.of_int sync_point.sync_skip_samples))
    /. sync_point.sync_rate

let feed_sync_page sync_point page =
  if Ogg.Page.granulepos page = sync_point.sync_granulepos then
    sync_point.sync_seen <- true;
  if sync_point.sync_seen then
    Ogg.Stream.put_page sync_point.sync_stream.os page

exception Found_sync

let feed_sync dec sync_points =
  let page = Ogg.Sync.read dec.sync in
  try
    List.iter
      (fun sync_point ->
        if Ogg.Page.serialno page = sync_point.sync_id then begin
          feed_sync_page sync_point page;
          raise Found_sync
        end)
      sync_points;
    assert false
  with Found_sync -> ()

let sync_forward dec sync_points sync_point =
  let rec skip (cur, skipped) =
    try
      let pos = Ogg.Stream.peek_granulepos sync_point.sync_stream.os in
      let total_samples = granuleconv sync_point.sync_stream.dec pos cur in
      let diff = Int64.to_int (Int64.sub total_samples cur) in
      if skipped + diff < sync_point.sync_skip_samples then begin
        Ogg.Stream.skip_packet sync_point.sync_stream.os;
        skip (total_samples, skipped + diff)
      end
      else
        sync_point.sync_stream.position <-
          (Int64.to_float sync_point.sync_stream.read_samples +. float skipped)
          /. sync_point.sync_rate
    with
      | Ogg.Out_of_sync -> skip (cur, skipped)
      | Ogg.Not_enough_data ->
          feed_sync dec sync_points;
          skip (cur, skipped)
  in
  skip (sync_point.sync_stream.read_samples, 0)

let seek ?(relative = false) dec time =
  if (not (can_seek dec)) || get_tracks dec = [] then raise Not_available;
  if eos dec then raise End_of_stream;
  let orig_time = get_position dec in
  if relative then
    log dec "Seeking to %.02f sec from current position at %.02f sec" time
      orig_time;
  let time = if relative then time +. orig_time else time in
  let time = if time < 0. then 0. else time in
  log dec "Seeking to absolute position at %.2f sec" time;
  let f id stream l =
    let sample_rate () =
      let x, y = sample_rate_priv stream.dec dec in
      float x /. float y
    in
    match stream.dec with
      | Audio_ba _ | Audio_both _ | Audio _ ->
          {
            sync_id = id;
            sync_stream = stream;
            sync_rate = sample_rate ();
            sync_seen = false;
            sync_granulepos = Int64.zero;
            sync_skip_samples = 0;
            sync_bytes = 0;
          }
          :: l
      | Video _ ->
          {
            sync_id = id;
            sync_stream = stream;
            sync_rate = sample_rate ();
            sync_seen = false;
            sync_granulepos = Int64.zero;
            sync_skip_samples = 0;
            sync_bytes = 0;
          }
          :: l
      | _ -> l
  in
  let sync_points = Hashtbl.fold f dec.streams [] in
  (* Resolve each sync_point. *)
  List.iter (find_seek_pos dec time) sync_points;
  (* Move all finished streams back to
   * streams. *)
  let f x y = Hashtbl.add dec.streams x y in
  Hashtbl.iter f dec.finished_streams;
  Hashtbl.clear dec.finished_streams;
  (* Now finally resync. *)
  let sync_bytes =
    let f cur sync_point =
      if sync_point.sync_bytes < cur then sync_point.sync_bytes else cur
    in
    List.fold_left f max_int sync_points
  in
  let page = sync_seek dec sync_bytes in
  (* First, reinitiate all ogg streams. *)
  let reiniate x =
    x.sync_stream.os <- Ogg.Stream.create ~serial:x.sync_id ();
    if Ogg.Page.serialno page = x.sync_id then feed_sync_page x page
  in
  List.iter reiniate sync_points;
  (* Get to the next sync point for
   * each streams. *)
  let resync x =
    sync_forward dec sync_points x;
    let fill () = feed dec in
    match x.sync_stream.dec with
      | Audio_ba d -> d.restart ~fill x.sync_stream.os
      | Audio_both (d, _) -> d.restart ~fill x.sync_stream.os
      | Audio d -> d.restart ~fill x.sync_stream.os
      | Video d -> d.restart ~fill x.sync_stream.os
      | _ -> ()
  in
  List.iter resync sync_points;
  let sync_time = get_position dec in
  log dec "Found nearest seek point at %.02f sec" sync_time;
  if relative then sync_time -. orig_time else sync_time

let seek ?relative dec time =
  try seek ?relative dec time
  with End_of_stream ->
    abort dec;
    raise End_of_stream

let incr_pos dec stream len =
  let x, y = sample_rate_priv stream.dec dec in
  let rate = float x /. float y in
  stream.position <- stream.position +. (float len /. rate)

let rec audio_info dec dtype =
  let _, _, stream = get_track dec dtype in
  try
    match stream.dec with
      | Audio_ba d -> d.info ()
      | Audio_both (d, _) -> d.info ()
      | Audio d -> d.info ()
      | _ -> raise Not_found
  with Ogg.Not_enough_data ->
    feed dec;
    audio_info dec dtype

let can_decode_ba dec dtype =
  let _, _, stream = get_track dec dtype in
  match stream.dec with Audio_ba _ | Audio_both _ -> true | _ -> false

let rec video_info dec dtype =
  let _, _, stream = get_track dec dtype in
  try match stream.dec with Video d -> d.info () | _ -> raise Not_found
  with Ogg.Not_enough_data ->
    feed dec;
    video_info dec dtype

let decode_audio_gen ~get_decoder ~length dec dtype f =
  let ended, id, stream = get_track dec dtype in
  try
    let f x =
      begin try incr_pos dec stream (length x.(0)) with _ -> ()
      end;
      f x
    in
    (get_decoder stream.dec).decode f
  with
  | ( End_of_stream
    (* In very rare cases (e.g. with a track that
     * does not have any data to decode), [Ogg.Not_enough_data]
     * may be raised at the end of the track instead of
     * [End_of_stream]. Thus, we also catch it here
     * but re-raise it if the track has not ended yet. *)
    | Ogg.Not_enough_data ) as e
  ->
    if ended then begin
      log dec "All data from stream %nx has been decoded" id;
      Hashtbl.remove dec.finished_streams id
      (* Reraise [Ogg.Not_enough_data] to feed the
       * decoder. *)
    end
    else if e = Ogg.Not_enough_data then raise e;
    if eos dec then raise End_of_stream

let decode_audio =
  let get_decoder = function
    | Audio d -> d
    | Audio_both (d, _) -> d
    | _ -> raise Not_available
  in
  let length = Array.length in
  decode_audio_gen ~get_decoder ~length

let decode_audio_ba =
  let get_decoder = function
    | Audio_ba d -> d
    | Audio_both (_, d) -> d
    | _ -> raise Not_available
  in
  let length = Bigarray.Array1.dim in
  decode_audio_gen ~get_decoder ~length

let decode_video dec dtype f =
  let ended, id, stream = get_track dec dtype in
  try
    let f x =
      incr_pos dec stream 1;
      f x
    in
    match stream.dec with Video d -> d.decode f | _ -> assert false
  with (End_of_stream | Ogg.Not_enough_data) as e ->
    if ended then begin
      log dec "All data from stream %nx has been decoded: dropping stream." id;
      Hashtbl.remove dec.finished_streams id
      (* Reraise [Ogg.Not_enough_data] to feed the
       * decoder. *)
    end
    else if e = Ogg.Not_enough_data then raise e;
    if eos dec then raise End_of_stream

let decode_rec g dec dtype f =
  let rec exec () =
    try g dec dtype f
    with Ogg.Not_enough_data ->
      feed dec;
      exec ()
  in
  exec ()

let decode_audio = decode_rec decode_audio
let decode_audio_ba = decode_rec decode_audio_ba
let decode_video = decode_rec decode_video
