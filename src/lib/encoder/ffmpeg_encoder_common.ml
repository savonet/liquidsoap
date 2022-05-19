(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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
  encode : Frame.t -> int -> int -> unit;
  can_split : unit -> bool;
  codec_attr : unit -> string option;
  bitrate : unit -> int option;
  video_size : unit -> (int * int) option;
}

type handler = {
  output : Avutil.output Avutil.container;
  audio_stream : encoder option;
  video_stream : encoder option;
  mutable started : bool;
}

type stream_data = {
  idx : Int64.t;
  mutable last_start : Int64.t;
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
      Stream.merge store { idx; ready_count = 0; last_start; ready = false }
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

let encode ~encoder frame start len =
  if not encoder.started then (
    ignore
      (Option.map (fun { mk_stream } -> mk_stream frame) encoder.audio_stream);
    ignore
      (Option.map (fun { mk_stream } -> mk_stream frame) encoder.video_stream));
  encoder.started <- true;
  ignore
    (Option.map (fun { encode } -> encode frame start len) encoder.audio_stream);
  ignore
    (Option.map (fun { encode } -> encode frame start len) encoder.video_stream)

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
    | `String layout -> `Int64 Avutil.Channel_layout.(get_id (find layout))
    | _ -> assert false)

let encoder ~mk_audio ~mk_video ffmpeg meta =
  let buf = Strings.Mutable.empty () in
  let make () =
    let options = Hashtbl.copy ffmpeg.Ffmpeg_format.other_opts in
    convert_options options;
    let write str ofs len =
      Strings.Mutable.add_subbytes buf str ofs len;
      len
    in
    let format = mk_format ffmpeg in
    let output =
      match ffmpeg.Ffmpeg_format.output with
        | `Stream ->
            if format = None then (
              match ffmpeg.Ffmpeg_format.format with
                | None -> failwith "Format is required!"
                | Some fmt ->
                    failwith
                      (Printf.sprintf
                         "No ffmpeg format could be guessed for format=%S" fmt));
            Av.open_output_stream ~opts:options write (Option.get format)
        | `Url url -> Av.open_output ?format ~opts:options url
    in
    let audio_stream =
      Option.map
        (fun _ -> mk_audio ~ffmpeg ~options output)
        ffmpeg.Ffmpeg_format.audio_codec
    in
    let video_stream =
      Option.map
        (fun _ -> mk_video ~ffmpeg ~options output)
        ffmpeg.Ffmpeg_format.video_codec
    in
    if Hashtbl.length options > 0 then
      failwith
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options options));
    { output; audio_stream; video_stream; started = false }
  in
  let encoder = ref (make ()) in
  let codec_attrs () =
    let encoder = !encoder in
    match (encoder.video_stream, encoder.audio_stream) with
      | Some v, Some a -> (
          match (v.codec_attr (), a.codec_attr ()) with
            | Some v, Some a -> Some (Printf.sprintf "%s,%s" v a)
            | _ -> None)
      | None, Some s | Some s, None -> s.codec_attr ()
      | None, None -> None
  in
  let bitrate () =
    let encoder = !encoder in
    Some
      (List.fold_left
         (fun cur -> function
           | None -> cur
           | Some s -> (
               match s.bitrate () with Some b -> cur + b | None -> cur))
         0
         [encoder.video_stream; encoder.audio_stream])
  in
  let video_size () =
    let encoder = !encoder in
    match encoder.video_stream with Some s -> s.video_size () | None -> None
  in
  let audio_sent = ref false in
  let video_sent = ref false in
  let init_encode frame start len =
    let encoder = !encoder in
    match ffmpeg.Ffmpeg_format.format with
      | Some "mp4" ->
          encode ~encoder frame start len;
          if encoder.video_stream <> None then (
            let d = Content.sub (Frame.video frame) start len in
            video_sent := !video_sent || not (Content.is_empty d))
          else video_sent := true;
          if encoder.audio_stream <> None then (
            let d = Content.sub (Frame.audio frame) start len in
            audio_sent := !audio_sent || not (Content.is_empty d))
          else audio_sent := true;
          if not (!audio_sent && !video_sent) then raise Encoder.Not_enough_data;
          Av.flush encoder.output;
          let init = Strings.Mutable.flush buf in
          (Some init, Strings.empty)
      | Some "webm" ->
          Av.flush encoder.output;
          let init = Strings.Mutable.flush buf in
          encode ~encoder frame start len;
          (Some init, Strings.Mutable.flush buf)
      | _ ->
          encode ~encoder frame start len;
          (None, Strings.Mutable.flush buf)
  in
  let split_encode frame start len =
    let encoder = !encoder in
    Av.flush encoder.output;
    let flushed = Strings.Mutable.flush buf in
    encode ~encoder frame start len;
    let encoded = Strings.Mutable.flush buf in
    match encoder.video_stream with
      | Some s when s.can_split () -> `Ok (flushed, encoded)
      | None -> `Ok (flushed, encoded)
      | _ -> `Nope (Strings.append flushed encoded)
  in
  let encode frame start len =
    encode ~encoder:!encoder frame start len;
    Strings.Mutable.flush buf
  in
  let insert_metadata m =
    let m =
      Hashtbl.fold (fun lbl v l -> (lbl, v) :: l) (Meta_format.to_metadata m) []
    in
    match (ffmpeg.Ffmpeg_format.output, ffmpeg.Ffmpeg_format.format) with
      | _ when not !encoder.started -> Av.set_output_metadata !encoder.output m
      | `Stream, Some "ogg" ->
          Av.close !encoder.output;
          encoder := make ();
          Av.set_output_metadata !encoder.output m
      | _ -> ()
  in
  insert_metadata meta;
  let stop () =
    Av.close !encoder.output;
    Strings.Mutable.flush buf
  in
  let hls =
    { Encoder.init_encode; split_encode; codec_attrs; bitrate; video_size }
  in
  { Encoder.insert_metadata; header = Strings.empty; hls; encode; stop }
