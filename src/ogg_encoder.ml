(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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

type track_data = 
  | Audio_data of audio data
  | Video_data of video data

type position = Unknown | Time of float

type 'a track_encoder = 'a data -> Ogg.Stream.t -> (Ogg.Page.t -> unit) -> unit
type page_end_time = Ogg.Page.t -> position
type header_encoder = Ogg.Stream.t -> Ogg.Page.t
type fisbone_packet = Ogg.Stream.t -> Ogg.Stream.packet option
type stream_start = Ogg.Stream.t -> Ogg.Page.t list
type end_of_stream = Ogg.Stream.t -> unit

type 'a stream = 
  {
    os                : Ogg.Stream.t;
    encoder           : 'a track_encoder;
    end_pos           : page_end_time;
    available         : Ogg.Page.t Queue.t;
    mutable remaining : (float*Ogg.Page.t) option;
    fisbone_data      : fisbone_packet; 
    start_page        : stream_start;
    stream_end        : end_of_stream
  } 

type track =
  | Audio_track of (audio stream)
  | Video_track of (video stream)

(** You may register new tracks on state Eos or Bos.
  * You can't register new track on state Streaming. 
  * You may finalize at any state, provided at least 
  * single track is registered. However, this is not
  * recommended. *)
type state = Eos | Streaming | Bos

type t =
  {
    id               : string;
    mutable skeleton : Ogg.Stream.t option;
    encoded          : Buffer.t;
    mutable position : float;
    tracks           : (nativeint,track) Hashtbl.t;
    mutable state    : state ;
  }

type data_encoder = 
  | Audio_encoder of audio track_encoder
  | Video_encoder of video track_encoder

type stream_encoder =
  {
    header_encoder : header_encoder;
    fisbone_packet : fisbone_packet;
    stream_start   : stream_start;
    data_encoder   : data_encoder;
    end_of_page    : page_end_time;
    end_of_stream  : end_of_stream
  }

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
    | Audio_track x -> x.start_page
    | Video_track x -> x.start_page

let remaining_of_ogg_track x =
  match x with
    | Audio_track x -> x.remaining
    | Video_track x -> x.remaining

let set_remaining_of_ogg_track x v =
  match x with
    | Audio_track x -> x.remaining <- v
    | Video_track x -> x.remaining <- v

(** As per specifications, we need a random injective sequence of
  * nativeint. This might not be assumed here, but chances are very low.. *)
let random_state = Random.State.make_self_init ()
let get_serial () = 
  Random.State.nativeint random_state (Nativeint.of_int 0x3FFFFFFF)

let state encoder = 
  encoder.state

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

let flush_pages os = 
  let rec f os l = 
    try
      f os (Ogg.Stream.flush_page os::l)
    with
      | Ogg.Not_enough_data -> 
         let compare x y = 
           compare (Ogg.Page.pageno x) (Ogg.Page.pageno y) 
         in
         List.sort compare l
  in
  f os []

let add_flushed_pages encoder os = 
  List.iter (add_page encoder) (flush_pages os)

let init_skeleton content =
  let serial = get_serial () in
  let os = Ogg.Stream.create ~serial () in
  Ogg.Stream.put_packet os (Ogg.Skeleton.fishead ());
  (* Output first page at beginning of content. *)
  let (h,v) = Ogg.Stream.get_page os in
  Buffer.add_string content h;
  Buffer.add_string content v;
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
    position = 0.;
    tracks   = Hashtbl.create 10;
    state    = Bos
  }

let register_track encoder track_encoder =
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
  (** Encode headers *) 
  let p = track_encoder.header_encoder os in
  add_page encoder p;
  let track = 
    match track_encoder.data_encoder with
      | Audio_encoder encoder -> 
         Audio_track 
           { 
             os = os; 
             encoder = encoder;
             end_pos = track_encoder.end_of_page;
             available = Queue.create ();
             remaining = None;
             fisbone_data = track_encoder.fisbone_packet;
             start_page = track_encoder.stream_start;
             stream_end = track_encoder.end_of_stream
           }
      | Video_encoder encoder -> 
         Video_track 
           { 
             os = os; 
             encoder = encoder;
             end_pos = track_encoder.end_of_page;
             available = Queue.create ();
             remaining = None;
             fisbone_data = track_encoder.fisbone_packet;
             start_page = track_encoder.stream_start;
             stream_end = track_encoder.end_of_stream
           }
  in
  Hashtbl.add encoder.tracks serial track;
  serial

(** Start streams, set state to Streaming. *)
let streams_start encoder =
  if Hashtbl.length encoder.tracks = 0 then
    log#f 4 "%s: Starting stream with no ogg track.." encoder.id;
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
          add_flushed_pages encoder os
      | None -> ()
  end;
  Hashtbl.iter
   (fun _ -> fun t ->
     let os = os_of_ogg_track t in
     let stream_start = stream_start_of_ogg_track t in
     List.iter (add_page encoder) (stream_start os))
   encoder.tracks;
  (** Finish skeleton stream now. *)
  begin
    match encoder.skeleton with
      | Some os -> 
         Ogg.Stream.put_packet os (Ogg.Skeleton.eos ());
         let p = Ogg.Stream.flush_page os in
         add_page encoder p;
      | None -> ()
  end;
  encoder.state <- Streaming

(** Is a track empty ?*)
let is_empty x =
  Ogg.Stream.eos x.os && x.remaining = None &&
  Queue.length x.available = 0 &&
   begin
    try
      let p = Ogg.Stream.get_page x.os in
      Queue.add p x.available;
      false
    with
      | Ogg.Not_enough_data -> true
   end

(** Get the least remaining page of all tracks. *) 
let least_remaining encoder = 
  let f _ t x = 
    match remaining_of_ogg_track t,x with
      | Some (new_pos,p),Some (_,pos,_) when new_pos <= pos 
            -> Some (t,new_pos,p)
      | Some (pos,p),None -> Some (t,pos,p)
      | _ -> x
  in
  Hashtbl.fold f encoder.tracks None  

(** Get the number of remaining page *)
let remaining_pages encoder = 
  let f _ t x = 
    if remaining_of_ogg_track t <> None then
      x+1
    else
      x
   in
   Hashtbl.fold f encoder.tracks 0

(** Fill output data with available pages.
  * The algorithm works as follow:
  *  + The encoder has a global position
  *  + Each time a page ends at a position
  *    that is ahead of the encoder position,
  *    the page is kept as remaining.
  *  + When there is one remaining page per
  *    track, we take the least of them and 
  *    add it. The encoder position is then 
  *    bumped.
  *  + As soon as the encoder's position is ahead
  *    of a page, then this page can be written *)
let add_available src encoder = 
 let rec fill src dst =
  (** First we check if there is a remaining
    * page that we can now output. *)
  if remaining_pages encoder = Hashtbl.length encoder.tracks then
  begin
   match least_remaining encoder with
     | None -> ()
     | Some (track,pos,p) -> 
          add_page encoder p;
          encoder.position <- pos;
          set_remaining_of_ogg_track track None
  end;
  (* Then, we proceed only if the track
   * is the only one left, or there is no 
   * remaining page. *)
  if Hashtbl.length encoder.tracks <= 1 ||
     src.remaining = None then
   try
     let p = 
       try
         Queue.take src.available
       with
         | Queue.Empty -> Ogg.Stream.get_page src.os 
     in
     let pos = src.end_pos p in
     begin
      match pos with
        (* Is the new position ahead ? *)
        | Time pos ->
           if pos > encoder.position then
             (* We don't output the page now 
              * since we want to let the possibility 
              * for another stream to add pages 
              * for a position between the current 
              * position and this new one. *)
             src.remaining <- Some (pos,p)
           else
            begin
             add_page encoder p;
             fill src dst
            end
        | Unknown ->
            add_page encoder p;
            fill src dst
     end
   with
     | Ogg.Not_enough_data -> ()
  in
  fill src encoder;
  if is_empty src then
    Hashtbl.remove encoder.tracks (Ogg.Stream.serialno src.os)

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
 let queue_add src p = 
   Queue.add p src.available
 in
 match data with
   | Audio_data x -> 
      begin
       match Hashtbl.find encoder.tracks id with
         | Audio_track t ->
            t.encoder x t.os (queue_add t);
            add_available t encoder
         | _ -> raise Invalid_data
      end
   | Video_data x -> 
      begin
       match Hashtbl.find encoder.tracks id with
         | Video_track t ->
            t.encoder x t.os (queue_add t);
            add_available t encoder
         | _ -> raise Invalid_data
      end

(** Finish a track.
  * Not all data will necessarily be outputed here. It is possible
  * that muxing needs also another track to end.. *)
let priv_end_of_track encoder id track =
  if encoder.state = Bos then
   begin
    log#f 4 "%s: Stream finished without calling streams_start !" encoder.id; 
    streams_start encoder
   end;
   match track with
       | Video_track x ->
           if not (Ogg.Stream.eos x.os) then
            begin
             log#f 4 "%s: Setting end of track %nx." encoder.id id; 
             x.stream_end x.os
            end;
           add_available x encoder
       | Audio_track x -> 
           if not (Ogg.Stream.eos x.os) then
            begin
             log#f 4 "%s: Setting end of track %nx." encoder.id id;
             x.stream_end x.os
            end;
           add_available x encoder

let end_of_track encoder id = 
  priv_end_of_track encoder id (Hashtbl.find encoder.tracks id)

(** Flush data from all tracks in the stream. *)
let flush encoder =
  begin
   match encoder.skeleton with
     | Some os -> add_flushed_pages encoder os
     | None -> ()
  end;
  while Hashtbl.length encoder.tracks > 0 do
    Hashtbl.iter (priv_end_of_track encoder) encoder.tracks
  done

(** Set end of stream on the encoder. *)
let eos encoder =
  if encoder.state <> Streaming then
    streams_start encoder;
  if Hashtbl.length encoder.tracks <> 0 then
    raise Invalid_usage ;
  log#f 4 "%s: Every ogg logical tracks have ended: setting end of stream." encoder.id;
  encoder.position <- 0.;
  encoder.state <- Eos

(** End all tracks in the stream. *)
let end_of_stream encoder =
  flush encoder;
  eos encoder

