(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

 (** Ogg Stream Encoder *)

let log = Dtools.Log.make ["ogg.encoder"]

exception Invalid_data
exception Invalid_usage

type audio = float array array
type video = RGB.t array array
type 'a data = 
  { 
    data   : 'a;
    offset : int;
    length : int
  }

type ogg_data = 
  | Audio_data of audio data
  | Video_data of video data

type ('a,'b) track_encoder = 'a -> 'b data -> Ogg.Stream.t -> unit
type header_encoder = Ogg.Stream.t -> Ogg.Page.t
type fisbone_data = Ogg.Stream.t -> Ogg.Stream.packet option
type stream_start = Ogg.Stream.t -> Ogg.Page.t
type end_of_stream = Ogg.Stream.t -> unit

type ('a,'b) track = 
  {
    os             : Ogg.Stream.t;
    encoder        : ('a,'b) track_encoder;
    fisbone_data   : fisbone_data; 
    stream_start   : stream_start;
    end_of_stream  : end_of_stream
  } 

type 'a ogg_track =
  | Audio_track of (('a,audio) track)
  | Video_track of (('a,video) track)

(** You may register new tracks on state Eos or Bos.
  * You can't register new track on state Streaming. 
  * You may finalize at any state, provided at least 
  * single track is registered. However, this is not
  * recommended. *)
type ogg_state = Eos | Streaming | Bos

type t =
  {
    id               : string;
    mutable skeleton : Ogg.Stream.t option;
    encoded          : Buffer.t;
    tracks           : (nativeint,t ogg_track) Hashtbl.t;
    mutable state    : ogg_state ;
  }

type ogg_data_encoder = 
  | Audio_encoder of ((t,audio) track_encoder)
  | Video_encoder of ((t,video) track_encoder)

type ogg_encoder = header_encoder*fisbone_data*ogg_data_encoder*end_of_stream

let os_of_ogg_track x = 
  match x with
    | Audio_track x -> x.os
    | Video_track x -> x.os

let fisbone_data_of_ogg_track x =
  match x with
    | Audio_track x -> x.fisbone_data
    | Video_track x -> x.fisbone_data

let stream_start_of_ogg_track x =
  match x with
    | Audio_track x -> x.stream_start
    | Video_track x -> x.stream_start

(** As per specifications, we need a random injective sequence of
  * nativeint. This might not be assumed here, but chances are very low.. *)
let random_state = Random.State.make_self_init ()
let get_serial () = 
  Random.State.nativeint random_state (Nativeint.of_int 0x3FFFFFFF)

let init_skeleton content = 
  let serial = get_serial () in
  let os = Ogg.Stream.create ~serial () in
  Ogg.Stream.put_packet os (Ogg.Skeleton.fishead ());
  (* Output first page at beginning of content. *)
  let (h,v) = Ogg.Stream.get_page os in
  Buffer.add_string content (h ^ v);
  os

let create ~skeleton id =
  let content = Buffer.create 1024 in
  let skeleton = 
    if skeleton then
      Some (init_skeleton content) 
    else
      None
  in
  {
    id       = id;
    skeleton = skeleton;
    encoded  = content;
    tracks   = Hashtbl.create 10;
    state    = Bos;
  }

(** Get and remove encoded data.. *)
let get_data encoder =
  let b = Buffer.contents encoder.encoded in
  Buffer.reset encoder.encoded;
  b

(** Peek encoded data without removing it. *)
let peek_data encoder =
  Buffer.contents encoder.encoded

(** Add an ogg page. *)
let add_page encoder (h,v) =
  Buffer.add_string encoder.encoded h;
  Buffer.add_string encoder.encoded v

let register_track encoder (header_enc,fisbone_data,stream_start,
                            track_enc,end_of_stream) =
  if encoder.state = Streaming then
   begin
    log#f 4 "%s: Invalid new track: ogg stream already started.." encoder.id;
    raise Invalid_usage
   end;
  if encoder.state = Eos then
   begin
    log#f 4 "%s: Starting new sequentialized ogg stream." encoder.id;
    if encoder.skeleton <> None then
      encoder.skeleton <- Some (init_skeleton encoder.encoded);
    encoder.state <- Bos;
   end;
  (** Initiate a new logical stream *)
  let serial = get_serial () in
  let os = Ogg.Stream.create ~serial () in
  (** Encoder headers *) 
  let (h,v) = header_enc os in
  Buffer.add_string encoder.encoded (h^v);
  let track = 
    match track_enc with
      | Audio_encoder encoder -> 
         Audio_track 
           { 
             os = os; 
             encoder = encoder;
             fisbone_data = fisbone_data;
             stream_start = stream_start;
             end_of_stream = end_of_stream
           }
      | Video_encoder encoder -> 
         Video_track 
           { 
             os = os; 
             encoder = encoder;
             fisbone_data = fisbone_data;
             stream_start = stream_start;
             end_of_stream = end_of_stream
           }
  in
  Hashtbl.add encoder.tracks serial track;
  serial

(** Start streams, set state to Streaming. *)
let streams_start encoder =
  if Hashtbl.length encoder.tracks = 0 then
    raise Invalid_usage ;
  log#f 4 "%s: Starting all streams" encoder.id;
  (** Add skeleton informations first. *)
  begin
    match encoder.skeleton with
      | Some os ->
         Hashtbl.iter 
          (fun _ -> fun x ->
            let sos = os_of_ogg_track x in
            let f = fisbone_data_of_ogg_track x in 
            match f sos with
              | Some p -> Ogg.Stream.put_packet os p;
              | None -> ())
          encoder.tracks;
          let data = Ogg.Stream.flush os in
          Buffer.add_string encoder.encoded data;
      | None -> ()
  end;
  Hashtbl.iter
   (fun _ -> fun t ->
     let os = os_of_ogg_track t in
     let stream_start = stream_start_of_ogg_track t in
     add_page encoder (stream_start os))
   encoder.tracks;
  (** Finish skeleton stream now. *)
  begin
    match encoder.skeleton with
      | Some os -> 
         Ogg.Stream.put_packet os (Ogg.Skeleton.eos ());
         let (h,v) = Ogg.Stream.flush_page os in
         Buffer.add_string encoder.encoded h;
         Buffer.add_string encoder.encoded v
      | None -> ()
  end;
  encoder.state <- Streaming

(** Encode data. Implicitely calls [streams_start]
  * if not called before. *)
let encode encoder id data =
 if encoder.state = Bos then
   streams_start encoder;
 if encoder.state = Eos then
   begin
    log#f 4 "%s: Cannot encode: ogg stream finished.." encoder.id;
    raise Invalid_usage
   end;
 let rec fill src dst = 
   try
     let (h,v) = Ogg.Stream.get_page src in
     Buffer.add_string dst (h^v);
     fill src dst
   with
     | Ogg.Not_enough_data -> ()
  in
  match data with
    | Audio_data x -> 
       begin
        match Hashtbl.find encoder.tracks id with
          | Audio_track t ->
             t.encoder encoder x t.os;
             fill t.os encoder.encoded
          | _ -> raise Invalid_data
       end
    | Video_data x -> 
       begin
        match Hashtbl.find encoder.tracks id with
          | Video_track t ->
             t.encoder encoder x t.os;
             fill t.os encoder.encoded
          | _ -> raise Invalid_data
       end

(** Flush data from all tracks in the stream. *)
let flush encoder = 
  let flush_track _ x = 
    let os = os_of_ogg_track x in
    let b = Ogg.Stream.flush os in
    Buffer.add_string encoder.encoded b
  in
  Hashtbl.iter flush_track encoder.tracks;
  Hashtbl.clear encoder.tracks;
  let b = Buffer.contents encoder.encoded in
  Buffer.reset encoder.encoded;
  b

(** Finish a track, set state to Eos if all tracks have ended. *)
let end_of_track encoder id =
  if encoder.state = Bos then
   begin
    log#f 4 "%s: Stream finished without calling streams_start !" encoder.id; 
    streams_start encoder
   end;
  let track = Hashtbl.find encoder.tracks id in
  log#f 4 "%s: Setting end of track %nx." encoder.id id;
  begin
    match track with
        | Video_track x -> 
            x.end_of_stream x.os;
            Buffer.add_string encoder.encoded (Ogg.Stream.flush x.os)
        | Audio_track x -> 
            x.end_of_stream x.os;
            Buffer.add_string encoder.encoded (Ogg.Stream.flush x.os)
  end;
  Hashtbl.remove encoder.tracks id;
  if Hashtbl.length encoder.tracks = 0 then
   begin
    log#f 4 "%s: Every ogg logical tracks have ended: setting end of stream." encoder.id;
    encoder.state <- Eos
   end

(** End all tracks in the stream. *)
let end_of_stream encoder = 
  Hashtbl.iter 
    (fun x -> fun _ -> end_of_track encoder x)
    encoder.tracks

