(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

(** FFMPEG encoder *)

let log = Ffmpeg_utils.log

type encoder = {
  mk_stream : Frame.t -> unit;
  encode : Frame.t -> unit;
  flush : unit -> unit;
  codec_attr : unit -> string option;
  bitrate : unit -> int option;
  video_size : unit -> (int * int) option;
}

type handler = {
  output : Avutil.output Avutil.container;
  streams : encoder Frame.Fields.t;
  format : (Avutil.output, Avutil.media_type) Avutil.format option;
  mutable insert_id3 :
    frame_position:int ->
    sample_position:int ->
    (string * string) list ->
    string option;
  started : bool Atomic.t;
}

type stream_data = {
  idx : Int64.t;
  mutable last_start : Int64.t;
  mutable position : Int64.t;
  mutable ready_count : int;
  mutable ready : bool;
}

module Stream = Weak.Make (struct
  type t = stream_data

  let equal x y = x.idx = y.idx
  let hash x = Int64.to_int x.idx
end)

(* We lazily store last_start when concatenating
   streams. The idea is to always have the greatest
   possible offset, for instance if we concatenate:
     a: ------>als
     v: ----------->vls
   the last start should be vls so we end up with:
     a: ------>     -------->
     v: ----------->-------->

   Also, if one or more stream is waiting for a key frame,
   we make the whole set of streams wait for the first key
   frame. This makes sure that we avoid e.g. starting an
   audio track with no keyframe before the video has started. *)
let mk_stream_store total_count =
  let store = Stream.create 1 in
  let add ~last_start ~ready idx =
    let data =
      Stream.merge store
        { idx; ready_count = 0; last_start; position = 0L; ready = false }
    in
    if ready then data.ready_count <- data.ready_count + 1;
    if data.ready_count = total_count then data.ready <- true;
    if data.last_start < last_start then data.last_start <- last_start;
    data
  in
  let remove data =
    let data = Stream.merge store data in
    if data.ready_count = 1 then Stream.remove store data
    else data.ready_count <- data.ready_count - 1
  in
  (add, remove)

let mk_format ffmpeg =
  match (ffmpeg.Ffmpeg_format.format, ffmpeg.Ffmpeg_format.output) with
    | short_name, `Url filename ->
        Av.Format.guess_output_format ~filename ?short_name ()
    | Some short_name, _ -> Av.Format.guess_output_format ~short_name ()
    | _ -> None

let encode ~encoder frame =
  if not (Atomic.exchange encoder.started true) then
    Frame.Fields.iter (fun _ { mk_stream } -> mk_stream frame) encoder.streams;
  Frame.Fields.iter (fun _ { encode } -> encode frame) encoder.streams

(* Convert ffmpeg-specific options. *)
let convert_options opts =
  let convert name fn =
    match Hashtbl.find_opt opts name with
      | None -> ()
      | Some v -> Hashtbl.replace opts name (fn v)
  in
  convert "sample_fmt" (function
    | `String fmt -> `Int Avutil.Sample_format.(get_id (find fmt))
    | _ -> assert false);
  convert "channel_layout" (function
    | `String layout ->
        `String Avutil.Channel_layout.(get_description (find layout))
    | _ -> assert false)

let encoder ~pos ~on_keyframe ~keyframes ~mk_streams ffmpeg meta =
  let buf = Strings.Mutable.empty () in
  let make () =
    let options = Hashtbl.copy ffmpeg.Ffmpeg_format.opts in
    convert_options options;
    let write str ofs len =
      Strings.Mutable.add_subbytes buf str ofs len;
      len
    in
    let format = mk_format ffmpeg in
    let interleaved =
      match ffmpeg.interleaved with
        | `Default -> 1 < List.length ffmpeg.streams
        | `True -> true
        | `False -> false
    in
    let output =
      match ffmpeg.Ffmpeg_format.output with
        | `Stream ->
            if format = None then (
              match ffmpeg.Ffmpeg_format.format with
                | None -> Lang_encoder.raise_error ~pos "Format is required!"
                | Some fmt ->
                    Lang_encoder.raise_error ~pos
                      (Printf.sprintf
                         "No ffmpeg format could be found for format=%S" fmt));
            Av.open_output_stream ~interleaved ~opts:options write
              (Option.get format)
        | `Url url -> Av.open_output ?format ~interleaved ~opts:options url
    in
    let streams = mk_streams output in
    if Hashtbl.length options > 0 then
      Lang_encoder.raise_error ~pos
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options options));
    {
      format;
      output;
      streams;
      insert_id3 = (fun ~frame_position:_ ~sample_position:_ _ -> None);
      started = Atomic.make false;
    }
  in
  let encoder = Atomic.make (make ()) in
  let codec_attrs () =
    let encoder = Atomic.get encoder in
    match
      Frame.Fields.fold
        (fun _ c cur ->
          match c.codec_attr () with Some c -> c :: cur | None -> cur)
        encoder.streams []
    with
      | [] -> None
      | l -> Some (String.concat "," l)
  in
  let bitrate () =
    let encoder = Atomic.get encoder in
    let ( + ) v v' =
      match (v, v') with
        | None, None -> None
        | None, Some v | Some v, None -> Some v
        | Some v, Some v' -> Some (v + v')
    in
    Frame.Fields.fold (fun _ c cur -> cur + c.bitrate ()) encoder.streams None
  in
  let video_size () =
    let encoder = Atomic.get encoder in
    match
      Frame.Fields.fold
        (fun _ stream cur ->
          match stream.video_size () with
            | Some (width, height) -> (width, height) :: cur
            | _ -> cur)
        encoder.streams []
    with
      | (width, height) :: [] -> Some (width, height)
      | _ -> None
  in
  let sent =
    Frame.Fields.map (fun _ -> ref false) (Atomic.get encoder).streams
  in
  let init ?id3_enabled ?id3_version () =
    let encoder = Atomic.get encoder in
    match Option.map Av.Format.get_output_name encoder.format with
      | Some "mpegts" ->
          if id3_enabled <> Some false then (
            let id3_version = Option.value ~default:3 id3_version in
            let time_base = Ffmpeg_utils.liq_audio_sample_time_base () in
            let stream =
              Av.new_data_stream ~time_base ~codec:`Timed_id3 encoder.output
            in
            encoder.insert_id3 <-
              (fun ~frame_position ~sample_position m ->
                if Atomic.get encoder.started then (
                  let tag = Utils.id3v2_of_metadata ~version:id3_version m in
                  let packet = Avcodec.Packet.create tag in
                  let position =
                    Int64.of_int
                      (Frame.audio_of_main
                         ((frame_position * Lazy.force Frame.size)
                         + sample_position))
                  in
                  Avcodec.Packet.set_pts packet (Some position);
                  Avcodec.Packet.set_dts packet (Some position);
                  Av.write_packet stream time_base packet);
                None);
            true)
          else false
      | Some "adts" | Some "mp3" | Some "ac3" | Some "eac3" ->
          if id3_enabled = Some false then
            Lang_encoder.raise_error ~pos "Format requires ID3 metadata!";
          encoder.insert_id3 <-
            (fun ~frame_position ~sample_position m ->
              Some
                (Encoder_utils.mk_hls_id3 ?id3_version ~frame_position
                   ~sample_position m));
          true
      | Some _ when id3_enabled = Some true ->
          Lang_encoder.raise_error ~pos
            "Format does not support HLS ID3 metadata!"
      | Some _ -> false
      | None -> Lang_encoder.raise_error ~pos "Format is required!"
  in
  let insert_id3 ~frame_position ~sample_position m =
    let encoder = Atomic.get encoder in
    encoder.insert_id3 ~frame_position ~sample_position m
  in
  let init_encode frame =
    let encoder = Atomic.get encoder in
    match ffmpeg.Ffmpeg_format.format with
      | Some "mp4" ->
          encode ~encoder frame;
          let frame =
            Frame.Fields.filter
              (fun _ c ->
                not
                  (Content_timed.Track_marks.is_data c
                  || Content_timed.Metadata.is_data c))
              frame
          in
          Frame.Fields.iter
            (fun field c ->
              match Frame.Fields.find_opt field sent with
                | None -> ()
                | Some sent -> sent := !sent || not (Content.is_empty c))
            frame;
          if Frame.Fields.exists (fun _ c -> not !c) sent then
            raise Encoder.Not_enough_data;
          Av.flush encoder.output;
          let init = Strings.Mutable.flush buf in
          (Some init, Strings.empty)
      | Some "webm" ->
          Av.flush encoder.output;
          let init = Strings.Mutable.flush buf in
          encode ~encoder frame;
          (Some init, Strings.Mutable.flush buf)
      | _ ->
          encode ~encoder frame;
          (None, Strings.Mutable.flush buf)
  in
  let split_encode frame =
    let encoder = Atomic.get encoder in
    let encoded_fields = List.map fst (Frame.Fields.bindings encoder.streams) in
    let can_split () =
      List.for_all
        (fun (field, keyframe) ->
          if not (List.mem field encoded_fields) then true
          else Atomic.get keyframe)
        keyframes
    in
    let flushed =
      if can_split () then Atomic.make (Some (Strings.Mutable.flush buf))
      else (
        let flushed = Atomic.make None in
        Atomic.set on_keyframe (fun () ->
            match (can_split (), Atomic.get flushed) with
              | true, None ->
                  Atomic.set flushed (Some (Strings.Mutable.flush buf))
              | _ -> ());
        flushed)
    in
    Fun.protect
      (fun () -> encode ~encoder frame)
      ~finally:(fun () -> Atomic.set on_keyframe (fun () -> ()));
    let encoded = Strings.Mutable.flush buf in
    match Atomic.get flushed with
      | Some flushed ->
          List.iter (fun (_, keyframe) -> Atomic.set keyframe false) keyframes;
          `Ok (flushed, encoded)
      | None -> `Nope encoded
  in
  let encode frame =
    encode ~encoder:(Atomic.get encoder) frame;
    Strings.Mutable.flush buf
  in
  let flush () =
    Frame.Fields.iter (fun _ { flush } -> flush ()) (Atomic.get encoder).streams
  in
  let encode_metadata m =
    let m = Frame.Metadata.Export.to_metadata m in
    let m = Frame.Metadata.append ffmpeg.metadata m in
    let m = Frame.Metadata.to_list m in
    match (ffmpeg.Ffmpeg_format.output, ffmpeg.Ffmpeg_format.format) with
      | _ when not (Atomic.get (Atomic.get encoder).started) ->
          Av.set_output_metadata (Atomic.get encoder).output m
      | `Stream, Some "ogg" ->
          flush ();
          Av.close (Atomic.get encoder).output;
          Atomic.set encoder (make ());
          Av.set_output_metadata (Atomic.get encoder).output m
      | _ -> ()
  in
  encode_metadata meta;
  let stop () =
    flush ();
    (try Av.close (Atomic.get encoder).output with _ -> ());
    Strings.Mutable.flush buf
  in
  let hls =
    {
      Encoder.init;
      init_encode;
      split_encode;
      insert_id3;
      codec_attrs;
      bitrate;
      video_size;
    }
  in
  {
    Encoder.encode_metadata;
    header = (fun () -> Strings.empty);
    hls;
    encode;
    stop;
  }
