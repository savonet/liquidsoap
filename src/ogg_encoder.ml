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

(** Internal type, depends on type t, which is defined later.. *)
type ('a,'b) internal_track_encoder = 'a -> 'b data -> Ogg.Stream.t -> unit
type position = Ogg.Page.t -> Int64.t
type header_encoder = Ogg.Stream.t -> Ogg.Page.t
type fisbone_packet = Ogg.Stream.t -> Ogg.Stream.packet option
type stream_start = Ogg.Stream.t -> string
type end_of_stream = Ogg.Stream.t -> unit

type ('a,'b) stream = 
  {
    os           : Ogg.Stream.t;
    encoder      : ('a,'b) internal_track_encoder;
    end_pos      : position;
    stream_rate  : int;
    mutable pos  : Int64.t;
    remaining    : (Int64.t*Ogg.Page.t) Queue.t;
    fisbone_data : fisbone_packet; 
    start_page   : stream_start;
    stream_end   : end_of_stream
  } 

type 'a track =
  | Audio_track of (('a,audio) stream)
  | Video_track of (('a,video) stream)

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
    mutable position : Int64.t;
    mutable latest   : nativeint;
    tracks           : (nativeint,t track) Hashtbl.t;
    mutable state    : state ;
  }

type 'a track_encoder = (t,'a) internal_track_encoder

type data_encoder = 
  | Audio_encoder of audio track_encoder
  | Video_encoder of video track_encoder

type stream_encoder =
  {
    header_encoder : header_encoder;
    fisbone_packet : fisbone_packet;
    stream_start   : stream_start;
    data_encoder   : data_encoder;
    rate           : int;
    end_of_page    : position;
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

(** Add a string. *)
let add_string encoder s = 
  Buffer.add_string encoder.encoded s

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
    position = Int64.zero;
    latest   = Nativeint.minus_one;
    tracks   = Hashtbl.create 10;
    state    = Bos;
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
  (** Encoder headers *) 
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
             stream_rate = track_encoder.rate;
             pos = Int64.zero;
             remaining = Queue.create ();
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
             stream_rate = track_encoder.rate;
             pos = Int64.zero;
             remaining = Queue.create ();
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
          add_string encoder data;
      | None -> ()
  end;
  Hashtbl.iter
   (fun _ -> fun t ->
     let os = os_of_ogg_track t in
     let stream_start = stream_start_of_ogg_track t in
     add_string encoder (stream_start os))
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

(** Encode data. Implicitely calls [streams_start]
  * if not called before. 
  *
  * The encoding works as follow:
  *  + The encoder has a global position
  *  + Each stream has a local position
  *  + Each time a page ends at a position
  *    that is ahead of the encoder position,
  *    then the encoder position is bumped,
  *    and we note that this stream was the last 
  *    one to increment.
  *  + The encoded pages are kept in a queue
  *  + As soon as the encoder's position is ahead
  *    of a queued page, then this page can be written
  *  + The queue also retains pages with negative
  *    granulepos. These pages contains less than one 
  *    packet, and should be written only when all 
  *    the required pages to decode at least one packet 
  *    are queued.
  *  + Special attention has been payed that, even though 
  *    a page has bumped the encoder's position, it should
  *    not be written immediately. Indeed, another stream
  *    may have a page increasing this position too, but 
  *    to a lower value. In this case, we want the other
  *    page to be written first. *)
let encode encoder id data =
 if encoder.state = Bos then
   streams_start encoder;
 if encoder.state = Eos then
   begin
    log#f 4 "%s: Cannot encode: ogg stream finished.." encoder.id;
    raise Invalid_usage
   end;
 let tmp_queue = Queue.create () in
 (* This functions writes all pages which contains at
  * least a packet, and end after the current position. *)
 let flush_queue q = 
   let test_page (pos,p) = 
     (* Is the page before the current position ? *)
     if pos <= encoder.position then
       (* Does the page contain at least one packet ? *)
       if pos >= Int64.zero then
        (* If yes, then we can writte all temporary pages
         * since they are before the current page. *)
        begin
          Queue.iter (fun (_,p) -> add_page encoder p) tmp_queue;
          Queue.clear tmp_queue;
          add_page encoder p
        end
       else
         (* If not, schedule it for later. *)
         Queue.add (pos,p) tmp_queue
     else
       (* If not, scheduler it for later. *)
       Queue.add (pos,p) tmp_queue
   in
   Queue.iter test_page q;
   Queue.clear q;
   Queue.transfer tmp_queue q;
 in
 let rec fill src dst =
  (* The current stream may bump the current 
   * encoder's position when it is the only one, 
   * or it was not the last one to update it. *) 
  if Hashtbl.length encoder.tracks = 1 ||
     (Ogg.Stream.serialno src.os) <> encoder.latest then
   try
     let p = Ogg.Stream.get_page src.os in
     let new_pos = src.end_pos p in
     let new_pos = Int64.mul (Int64.of_int src.stream_rate) new_pos in
     (* Is the new position ahead ? *)
     if new_pos >= encoder.position then
      begin
       (* Add the page to the queue, but do not
        * update the encoder's position yet. 
        * We don't update it now since we want
        * to let the possibility for another
        * stream to add pages for a position 
        * between the current position and 
        * this new one. *)
       Queue.add (new_pos,p) src.remaining;
       (* Write all pages that are waiting *)
       flush_queue src.remaining;
       (* Update positions only now.. *)
       src.pos <- new_pos;
       encoder.latest <- Ogg.Stream.serialno src.os;
       encoder.position <- new_pos
     end
     else
      begin
       Queue.add (new_pos,p) src.remaining;
       if new_pos >= Int64.zero then
         (* If the page contains data, try
          * to flush remaining pages, in particular
          * pages which previously ad a negative 
          * granulepos. *)
         flush_queue src.remaining;
       fill src dst
      end
   with
     | Ogg.Not_enough_data -> ()
  in
  match data with
    | Audio_data x -> 
       begin
        match Hashtbl.find encoder.tracks id with
          | Audio_track t ->
             t.encoder encoder x t.os;
             fill t encoder
          | _ -> raise Invalid_data
       end
    | Video_data x -> 
       begin
        match Hashtbl.find encoder.tracks id with
          | Video_track t ->
             t.encoder encoder x t.os;
             fill t encoder
          | _ -> raise Invalid_data
       end

(** Flush data from all tracks in the stream. *)
let flush encoder = 
  let flush_track _ x = 
    let os = os_of_ogg_track x in
    let b = Ogg.Stream.flush os in
    add_string encoder b
  in
  begin
   match encoder.skeleton with
     | Some os -> 
         let b = Ogg.Stream.flush os in
         add_string encoder b
     | None -> ()
  end;
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
            x.stream_end x.os;
            add_string encoder (Ogg.Stream.flush x.os)
        | Audio_track x -> 
            x.stream_end x.os;
            add_string encoder (Ogg.Stream.flush x.os)
  end;
  Hashtbl.remove encoder.tracks id

(** Set end of stream on the encoder. *)
let eos encoder =
  if Hashtbl.length encoder.tracks <> 0 then
    raise Invalid_usage ;
  log#f 4 "%s: Every ogg logical tracks have ended: setting end of stream." encoder.id;
   begin
    match encoder.skeleton with
      | Some os ->
          if not (Ogg.Stream.eos os) then
            Ogg.Stream.put_packet os (Ogg.Skeleton.eos ());
          let b = Ogg.Stream.flush os in
          add_string encoder b
      | None -> ()
   end;
  encoder.position <- Int64.zero;
  encoder.state <- Eos

(** End all tracks in the stream. *)
let end_of_stream encoder =
  Hashtbl.iter 
    (fun x -> fun _ -> end_of_track encoder x)
    encoder.tracks;
  eos encoder

