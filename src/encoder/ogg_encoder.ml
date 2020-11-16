(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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
  encode : Ogg_muxer.t -> nativeint -> Frame.t -> int -> int -> unit;
  reset : Ogg_muxer.t -> Meta_format.export_metadata -> nativeint;
  mutable id : nativeint option;
}

let audio_encoders = Hashtbl.create 3
let theora_encoder = ref None

(** Helper to encode audio *)
let encode_audio ~channels ~src_freq ~dst_freq () =
  let samplerate_converter = Audio_converter.Samplerate.create channels in
  (* start and len are in master ticks. *)
  let encode encoder id frame start len =
    let b = AFrame.pcm frame in
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
        { Ogg_muxer.data = Audio.to_array buf; offset = start; length = len }
    in
    Ogg_muxer.encode encoder id data
  in
  encode

(** Helper to encode video. *)
let encode_video encoder id frame start len =
  let data = VFrame.yuv420p frame in
  let start = Frame.video_of_master start in
  let len = Frame.video_of_master len in
  let data =
    Ogg_muxer.Video_data { Ogg_muxer.data; offset = start; length = len }
  in
  Ogg_muxer.encode encoder id data

let encoder_name = function
  | Ogg_format.Vorbis _ -> "vorbis"
  | Ogg_format.Opus _ -> "opus"
  | Ogg_format.Flac _ -> "flac"
  | Ogg_format.Speex _ -> "speex"

let get_encoder name =
  try Hashtbl.find audio_encoders name
  with Not_found ->
    Ogg_muxer.log#important "Could not find any %s encoder." name;
    raise Not_found

let encoder { Ogg_format.audio; video } =
  ignore (Option.map (fun p -> get_encoder (encoder_name p)) audio);
  ignore (Option.map (fun _ -> assert (!theora_encoder <> None)) video);
  fun name meta ->
    let tracks = [] in
    let tracks =
      match audio with
        | Some params ->
            let enc = get_encoder (encoder_name params) in
            enc params :: tracks
        | None -> tracks
    in
    let tracks =
      match video with
        | Some params ->
            let enc = Option.get !theora_encoder in
            enc params :: tracks
        | None -> tracks
    in
    (* We add a skeleton only
     * if there are more than one stream for now. *)
    let skeleton = List.length tracks > 1 in
    let ogg_enc = Ogg_muxer.create ~skeleton name in
    let rec enc =
      { Encoder.insert_metadata; hls; encode; header = Strings.empty; stop }
    and streams_start () =
      let f track =
        match track.id with
          | Some _ -> ()
          | None -> track.id <- Some (track.reset ogg_enc meta)
      in
      List.iter f tracks;
      Ogg_muxer.streams_start ogg_enc;
      enc.Encoder.header <- Ogg_muxer.get_header ogg_enc
    and encode frame start len =
      (* We do a lazy start, to 
       * avoid empty streams at beginning.. *)
      if Ogg_muxer.state ogg_enc <> Ogg_muxer.Streaming then (
        streams_start ();
        enc.Encoder.header <- Ogg_muxer.get_header ogg_enc );
      let f track =
        track.encode ogg_enc (Option.get track.id) frame start len
      in
      List.iter f tracks;
      Ogg_muxer.get_data ogg_enc
    and hls =
      {
        Encoder.init_encode = (fun f o l -> (None, encode f o l));
        split_encode = (fun f o l -> `Ok (Strings.empty, encode f o l));
        codec_attrs = (fun () -> None);
        bitrate = (fun () -> None);
        video_size = (fun () -> None);
      }
    and ogg_stop () =
      let f track = track.id <- None in
      List.iter f tracks;
      if Ogg_muxer.state ogg_enc = Ogg_muxer.Streaming then (
        Ogg_muxer.end_of_stream ogg_enc;
        enc.Encoder.header <- Strings.empty )
    and stop () =
      ogg_stop ();
      enc.Encoder.header <- Strings.empty;
      Ogg_muxer.get_data ogg_enc
    and insert_metadata m =
      ogg_stop ();
      let f track = track.id <- Some (track.reset ogg_enc m) in
      List.iter f tracks;
      enc.Encoder.header <- Ogg_muxer.get_header ogg_enc
    in
    enc

let () =
  Encoder.plug#register "OGG" (function
    | Encoder.Ogg m -> Some (encoder m)
    | _ -> None)
