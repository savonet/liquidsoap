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

 (** Ogg stream demuxer *)

let log = Dtools.Log.make ["ogg.demuxer"]

type metadata = string*((string*string) list)
type 'a decoder = ('a*(metadata option) -> unit) -> unit
type audio = (float array array)*int
type video_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
(** Only supported for now: plannar YUV formats. *)
type video_format =
   | Yuvj_420   (* Planar YCbCr 4:2:0. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_422   (* Planar YCbCr 4:2:2. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
   | Yuvj_444   (* Planar YCbCr 4:4:4. Each component is an uint8_t,
                 * luma and chroma values are full range (0x00 .. 0xff) *)
type video =
 {
    format    : video_format;
    fps       : float; (** Video frames per second *)
    width   : int; (** Width of the Y' luminance plane *)
    height  : int; (** Height of the luminance plane *)
    y_stride  : int; (** Length, in bytes, per line *)
    uv_stride : int; (** Length, in bytes, per line *)
    y : video_data; (** luminance data *)
    u : video_data; (** Cb data *)
    v : video_data; (** Cr data *)
 }
type decoders =
    | Video of video decoder
    | Audio of audio decoder
    | Unknown

type stream = Ogg.Stream.t*(bool ref)*decoders
type t =
{
  sync        : Ogg.Sync.t;
  mutable eos : bool;
  streams     : (nativeint,stream) Hashtbl.t;
}

type track = Audio_track | Video_track

exception Internal of Ogg.Page.t
exception Exit of nativeint*Ogg.Stream.t*decoders
exception Track of ((bool ref)*(decoders))
exception Invalid_stream
exception End_of_stream

let ogg_decoders : ((Ogg.Stream.packet -> bool)*
                    (Ogg.Stream.t -> decoders)) 
  Plug.plug =
    Plug.create ~doc:"Methods for decoding ogg streams." "ogg formats"

let eos dec = 
  dec.eos

let test page = 
  let serial = Ogg.Page.serialno page in
  log#f 5 "Found a ogg logical stream, serial: %nx" serial;
  let os = Ogg.Stream.create ~serial () in
  Ogg.Stream.put_page os page ;
  (* Get first packet *)
  let packet = Ogg.Stream.peek_packet os in
  try
    List.iter
      (fun (format,(check,decode)) ->
           log#f 5 "Trying ogg/%s format" format ;
           if check packet then
             (log#f 5 "ogg/%s format detected for stream %nx" 
                format serial ;
              raise (Exit (serial,os,decode os)))
           else ())
      ogg_decoders#get_all;
    log#f 5 "Couldn't find a decoder for ogg logical \
                 stream with serial %nx" serial;
    raise (Exit (serial,os,Unknown))
  with
    | Exit (s,o,d) -> s,o,d

let feed_page decoder page =
  let serial = Ogg.Page.serialno page in
  try
    let (os,eos,dec) = Hashtbl.find decoder.streams serial in
    if dec <> Unknown then
      Ogg.Stream.put_page os page ;
    if Ogg.Page.eos page then
      begin
        log#f 5 "Reached last page of logical stream %nx" serial;
        Hashtbl.remove decoder.streams serial;
        eos := true;
        if Hashtbl.length decoder.streams = 0 then
          decoder.eos <- true
      end
    with
      | Not_found ->
          log#f 5 "Couldn't find a decoder for page in stream %nx" serial;
          raise Invalid_stream

let feed decoder =
  if decoder.eos then
    raise End_of_stream ; 
  let page = Ogg.Sync.read decoder.sync in
  feed_page decoder page 

let parse dec =
    assert(not dec.eos);
    let rec parse () = 
      try
        (** Get First page *)
        let page = Ogg.Sync.read dec.sync in
        (** Check wether this is a b_o_s *)
        if not (Ogg.Page.bos page) then raise (Internal page); 
        let serial,os,decoder = test page in
        (* Should not happen *)
        if (Hashtbl.mem dec.streams serial) then
          raise Invalid_stream;
        Hashtbl.add dec.streams serial (os,ref false,decoder);
        parse () 
      with
        | Internal p ->
            feed_page dec p 
    in
    parse ();
    dec

let init sync = 
  let streams = Hashtbl.create 2 in
  parse { sync = sync; eos = false; streams = streams }

let reset dec = 
  Hashtbl.clear dec.streams;
  dec.eos <- false;
  ignore(parse dec)

let frame_meta_of_meta v =
  match v with
    | Some (v,l) ->
       let metas = Hashtbl.create 10 in
       List.iter (fun (x,y) -> 
               Hashtbl.add metas (String.lowercase x) y)
            l;
       Hashtbl.add metas "vendor" v;
       Some metas
    | None -> None

let get_track dtype dec =
  (* Only decode first audio track for now.. *)
  let test _ (_,eos,decoder) =
    (* We only support one audio track for now.. *) 
    match decoder with
      | Audio d when dtype = Audio_track -> raise (Track (eos,Audio d))
      | Video d when dtype = Video_track -> raise (Track (eos,Video d))
      | _ -> ()
  in
  try
    Hashtbl.iter test dec.streams;
    raise Not_found
  with
    | Track t -> t

let has_track dtype dec = 
  try
    let _ = get_track dtype dec in
    true
  with
    | Not_found -> false

let decode_audio dec f = 
  let (eos,d) = get_track Audio_track dec in
  try
    let f (x,y) = 
      f (x,frame_meta_of_meta y)
    in
    match d with
      | Audio d -> d f 
      | _ -> assert false
  with
    | Ogg.Not_enough_data ->
        if !eos then
          raise End_of_stream ;
        raise Ogg.Not_enough_data

let decode_video dec f =
  let (eos,d) = get_track Video_track dec in
  try
    let f (x,y) =
      f (x,frame_meta_of_meta y)
    in
    match d with
      | Video d -> d f
      | _ -> assert false
  with
    | Ogg.Not_enough_data ->
        if !eos then
          raise End_of_stream ;
        raise Ogg.Not_enough_data

let decode_rec g dec f =
  let rec exec () =
    try
      g dec f
    with
      | Ogg.Not_enough_data ->
          feed dec; exec ()
  in
  exec ()

let decode_audio_rec = decode_rec decode_audio
let decode_video_rec = decode_rec decode_video
