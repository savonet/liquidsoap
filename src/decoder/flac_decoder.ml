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

(** Decode and read metadata from flac files. *)

let log = Log.make ["decoder"; "flac"]

exception End_of_stream

module Make (Generator : Generator.S_Asio) = struct
  let create_decoder input =
    let resampler = Rutils.create_audio () in
    let read = input.Decoder.read in
    let seek =
      match input.Decoder.lseek with
        | Some f -> Some (fun len -> ignore (f (Int64.to_int len)))
        | None -> None
    in
    let tell =
      match input.Decoder.tell with
        | Some f -> Some (fun () -> Int64.of_int (f ()))
        | None -> None
    in
    let length =
      match input.Decoder.length with
        | Some f -> Some (fun () -> Int64.of_int (f ()))
        | None -> None
    in
    let dummy_c =
      Flac.Decoder.get_callbacks ?seek ?tell ?length read (fun _ -> ())
    in
    let decoder = Flac.Decoder.create dummy_c in
    let decoder, info, _ = Flac.Decoder.init decoder dummy_c in
    let sample_freq, _ =
      (info.Flac.Decoder.sample_rate, info.Flac.Decoder.channels)
    in
    let processed = ref Int64.zero in
    {
      Decoder.seek =
        (fun ticks ->
          let duration = Frame.seconds_of_master ticks in
          let samples = Int64.of_float (duration *. float sample_freq) in
          let pos = Int64.add !processed samples in
          let c =
            Flac.Decoder.get_callbacks ?seek ?tell ?length read (fun _ -> ())
          in
          let ret = Flac.Decoder.seek decoder c pos in
          if ret = true then (
            processed := pos;
            ticks )
          else (
            match Flac.Decoder.state decoder c with
              | `Seek_error ->
                  if Flac.Decoder.flush decoder c then 0
                  else
                    (* Flushing failed, we are in an unknown state.. *)
                    raise End_of_stream
              | _ -> 0 ));
      decode =
        (fun gen ->
          let c =
            Flac.Decoder.get_callbacks ?seek ?tell ?length read (fun data ->
                let data = Audio.of_array data in
                let len = try Audio.length data with _ -> 0 in
                processed := Int64.add !processed (Int64.of_int len);
                let content =
                  resampler ~audio_src_rate:(float sample_freq) data
                in
                Generator.set_mode gen `Audio;
                Generator.put_audio gen content 0 (Audio.length content))
          in
          match Flac.Decoder.state decoder c with
            | `Search_for_metadata | `Read_metadata | `Search_for_frame_sync
            | `Read_frame ->
                Flac.Decoder.process decoder c
            | _ -> raise End_of_stream);
    }
end

(** Configuration keys for flac. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "flac")
    "Mime-types used for guessing FLAC format" ~d:["audio/x-flac"]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "flac")
    "File extensions used for guessing FLAC format" ~d:["flac"]

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered (G)
module D = Make (G)

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  Buffered.file_decoder filename kind D.create_decoder generator

(* Get the number of channels of audio in an MP3 file.
 * This is done by decoding a first chunk of data, thus checking
 * that libmad can actually open the file -- which doesn't mean much. *)
let get_type filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o640 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd write fd in
      let info = h.Flac.Decoder.File.info in
      let rate, channels =
        (info.Flac.Decoder.sample_rate, info.Flac.Decoder.channels)
      in
      log#info "Libflac recognizes %S as FLAC (%dHz,%d channels)." filename rate
        channels;
      { Frame.audio = channels; video = 0; midi = 0 })

let () =
  Decoder.file_decoders#register "FLAC"
    ~sdoc:
      "Use libflac to decode any file if its MIME type or file extension is \
       appropriate." (fun ~metadata:_ filename kind ->
      if
        not
          (Decoder.test_file ~mimes:mime_types#get
             ~extensions:file_extensions#get ~log filename)
      then None
      else if
        kind.Frame.audio = Frame.Variable
        || kind.Frame.audio = Frame.Succ Frame.Variable
        ||
        (* libmad always respects the first two kinds *)
        if Frame.type_has_kind (get_type filename) kind then true
        else (
          log#important "File %S has an incompatible number of channels."
            filename;
          false )
      then Some (fun () -> create_file_decoder filename kind)
      else None)

module D_stream = Make (Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register "FLAC"
    ~sdoc:"Use libflac to decode any stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime mime_types#get
        (* Check that it is okay to have zero video and midi,
         * and at least one audio channel. *)
        && Frame.Zero <: kind.Frame.video
        && Frame.Zero <: kind.Frame.midi
        && kind.Frame.audio <> Frame.Zero
      then
        (* In fact we can't be sure that we'll satisfy the content
         * kind, because the MP3 stream might be mono or stereo.
         * For now, we let this problem result in an error at
         * decoding-time. Failing early would only be an advantage
         * if there was possibly another plugin for decoding
         * correctly the stream (e.g. by performing conversions). *)
        Some D_stream.create_decoder
      else None)

let log = Log.make ["metadata"; "flac"]

let get_tags file =
  if
    not
      (Decoder.test_file ~mimes:mime_types#get ~extensions:file_extensions#get
         ~log file)
  then raise Not_found;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o640 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd write fd in
      match h.Flac.Decoder.File.comments with Some (_, m) -> m | None -> [])

let () = Request.mresolvers#register "FLAC" get_tags

let check filename =
  match Configure.file_mime with
    | Some f -> List.mem (f filename) mime_types#get
    | None -> (
        try
          ignore (get_type filename);
          true
        with _ -> false )

let duration file =
  if not (check file) then raise Not_found;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o640 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd write fd in
      let info = h.Flac.Decoder.File.info in
      match info.Flac.Decoder.total_samples with
        | x when x = Int64.zero -> raise Not_found
        | x -> Int64.to_float x /. float info.Flac.Decoder.sample_rate)

let () = Request.dresolvers#register "FLAC" duration
