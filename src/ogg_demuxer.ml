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

 (** Ogg stream demuxer *)

let log = Dtools.Log.make ["ogg.demuxer"]

module Generator = Float_pcm.Generator

type metadata = string*((string*string) list)
type 'a decoder = ('a*(metadata option) -> unit) -> unit
type audio = (float array array)*int
type video_data = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
type video =
 {
    fps       : float; (** Video frames per second *)
    y_width   : int; (** Width of the Y' luminance plane *)
    y_height  : int; (** Height of the luminance plane *)
    y_stride  : int; (** Length, in bytes, per line *)
    uv_width  : int; (** Width of the Cb and Cr chroma planes *)
    uv_height : int; (** Height of the chroma planes *)
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

let test page = 
  let serial = Ogg.Page.serialno page in
  log#f 4 "Found a ogg logical stream, serial: %nx" serial;
  let os = Ogg.Stream.create ~serial () in
  Ogg.Stream.put_page os page ;
  (* Get first packet *)
  let packet = Ogg.Stream.peek_packet os in
  try
    List.iter
      (fun (format,(check,decode)) ->
           log#f 4 "Trying ogg/%s format" format ;
           if check packet then
             raise (Exit (serial,os,decode os))
           else ())
      ogg_decoders#get_all;
    log#f 4 "Couldn't find a decoder for ogg logical \
                 stream with serial %nx" serial;
    raise (Exit (serial,os,Unknown))
  with
    | Exit (s,o,d) -> s,o,d

let feed_page decoder page =
  let serial = Ogg.Page.serialno page in
  try
    let (os,eos,dec) = Hashtbl.find decoder.streams serial in
    if dec <> Unknown then
        Ogg.Stream.put_page os page
    else
      log#f 7 "No decoder for page of stream %nx" serial;
    if Ogg.Page.eos page then
      begin
        log#f 4 "Reached last page of logical stream %nx" serial;
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
  let rec ret () =
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
            raise End_of_stream;
          feed dec; ret ()
  in
  ret ()
 
let decode_video dec f =
  let (eos,d) = get_track Video_track dec in
  let rec ret () =
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
            raise End_of_stream;
          feed dec; ret ()
  in
  ret ()
