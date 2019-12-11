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

(** OGG encoder *)

type track = {
  encode: Ogg_muxer.t -> nativeint -> Frame.content -> int -> int -> unit;
  reset: Ogg_muxer.t -> Meta_format.export_metadata -> nativeint;
  mutable id: nativeint option;
}

let encoders = Hashtbl.create 3

(** Helper to encode audio *)
let encode_audio ~channels ~src_freq ~dst_freq () =
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  (* start and len are in master ticks. *)
  let encode encoder id content start len =
    let b = content.Frame.audio in
    let start = Frame.audio_of_master start in
    let len = Frame.audio_of_master len in
    let buf, start, len =
      if src_freq <> dst_freq then (
        let b =
          Audio_converter.Samplerate.resample samplerate_converter
            (dst_freq /. src_freq) (Audio.sub b start len)
        in
        (b, 0, Audio.length b) )
      else (b, start, len)
    in
    let data =
      Ogg_muxer.Audio_data
        {Ogg_muxer.data= Audio.to_array buf; offset= start; length= len}
    in
    Ogg_muxer.encode encoder id data
  in
  encode

(** Helper to encode video. *)
let encode_video encoder id content start len =
  let data = content.Frame.video in
  let start = Frame.video_of_master start in
  let len = Frame.video_of_master len in
  let data =
    Ogg_muxer.Video_data {Ogg_muxer.data= data.(0); offset= start; length= len}
  in
  Ogg_muxer.encode encoder id data

let encoder_name = function
  | Ogg_format.Vorbis _ ->
      "vorbis"
  | Ogg_format.Opus _ ->
      "opus"
  | Ogg_format.Flac _ ->
      "flac"
  | Ogg_format.Theora _ ->
      "theora"
  | Ogg_format.Speex _ ->
      "speex"

let get_encoder tr =
  let name = encoder_name tr in
  try Hashtbl.find encoders name
  with Not_found ->
    Ogg_muxer.log#important "Could not find any %s encoder." name ;
    raise Not_found

let encoder ogg =
  let check_track t =
    let (_ : Ogg_format.item -> track) = get_encoder t in
    ()
  in
  List.iter check_track ogg ;
  fun name meta ->
    (* We add a skeleton only
     * if there are more than one stream for now. *)
    let skeleton = List.length ogg > 1 in
    let create_track cur tr =
      let create = get_encoder tr in
      create tr :: cur
    in
    let tracks = List.fold_left create_track [] ogg in
    let ogg_enc = Ogg_muxer.create ~skeleton name in
    let rec enc =
      {Encoder.insert_metadata; encode; header= Strings.empty; stop}
    and streams_start () =
      let f track =
        match track.id with
          | Some _ ->
              ()
          | None ->
              track.id <- Some (track.reset ogg_enc meta)
      in
      List.iter f tracks ;
      Ogg_muxer.streams_start ogg_enc ;
      enc.Encoder.header <- Ogg_muxer.get_header ogg_enc
    and encode frame start len =
      (* We do a lazy start, to 
       * avoid empty streams at beginning.. *)
      if Ogg_muxer.state ogg_enc <> Ogg_muxer.Streaming then (
        streams_start () ;
        enc.Encoder.header <- Ogg_muxer.get_header ogg_enc ) ;
      let _, content = Frame.content frame start in
      let f track =
        track.encode ogg_enc (Utils.get_some track.id) content start len
      in
      List.iter f tracks ; Ogg_muxer.get_data ogg_enc
    and ogg_stop () =
      let f track = track.id <- None in
      List.iter f tracks ;
      if Ogg_muxer.state ogg_enc = Ogg_muxer.Streaming then (
        Ogg_muxer.end_of_stream ogg_enc ;
        enc.Encoder.header <- Strings.empty )
    and stop () =
      ogg_stop () ;
      enc.Encoder.header <- Strings.empty ;
      Ogg_muxer.get_data ogg_enc
    and insert_metadata m =
      ogg_stop () ;
      let f track = track.id <- Some (track.reset ogg_enc m) in
      List.iter f tracks ;
      enc.Encoder.header <- Ogg_muxer.get_header ogg_enc
    in
    enc

let () =
  Encoder.plug#register "OGG" (function
    | Encoder.Ogg m ->
        Some (encoder m)
    | _ ->
        None)
