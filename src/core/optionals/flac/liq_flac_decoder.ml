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

open Mm

(** Decode and read metadata from flac files. *)

let log = Log.make ["decoder"; "flac"]

exception End_of_stream

let create_decoder input =
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
  let write_ref = ref (fun _ -> ()) in
  let write v =
    let fn = !write_ref in
    fn v
  in
  let decoder, info, _ =
    Flac.Decoder.create ?seek ?tell ?length ~read ~write ()
  in
  let samplerate, _ =
    (info.Flac.Decoder.sample_rate, info.Flac.Decoder.channels)
  in
  let processed = ref Int64.zero in
  {
    Decoder.seek =
      (fun ticks ->
        let duration = Frame.seconds_of_main ticks in
        let samples = Int64.of_float (duration *. float samplerate) in
        let pos = Int64.add !processed samples in
        let ret = Flac.Decoder.seek decoder pos in
        if ret = true then (
          processed := pos;
          ticks)
        else (
          match Flac.Decoder.state decoder with
            | `Seek_error ->
                if Flac.Decoder.flush decoder then 0
                else
                  (* Flushing failed, we are in an unknown state.. *)
                  raise End_of_stream
            | _ -> 0));
    decode =
      (fun buffer ->
        (write_ref :=
           fun data ->
             let len = try Audio.length data with _ -> 0 in
             processed := Int64.add !processed (Int64.of_int len);
             buffer.Decoder.put_pcm ~samplerate data);
        match Flac.Decoder.state decoder with
          | `Search_for_metadata | `Read_metadata | `Search_for_frame_sync
          | `Read_frame ->
              Flac.Decoder.process decoder
          | _ -> raise End_of_stream);
    eof = (fun _ -> ());
    close = (fun _ -> ());
  }

(** Configuration keys for flac. *)
let mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "flac")
    "Mime-types used for guessing FLAC format"
    ~d:["audio/flac"; "audio/x-flac"]

let file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "flac")
    "File extensions used for guessing FLAC format" ~d:["flac"]

let priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "flac")
    "Priority for the flac decoder" ~d:1

(* Get the number of channels of audio in a flac file.
   This is done by decoding a first chunk of data, thus checking
   that libmad can actually open the file -- which doesn't mean much. *)
let file_type filename =
  let fd = Decoder.openfile filename in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd ~write fd in
      let info = h.Flac.Decoder.File.info in
      let rate, channels =
        (info.Flac.Decoder.sample_rate, info.Flac.Decoder.channels)
      in
      log#important "Libflac recognizes %s as FLAC (%dHz,%d channels)."
        (Lang_string.quote_string filename)
        rate channels;
      Some
        (Frame.Fields.make
           ~audio:(Content.Audio.format_of_channels channels)
           ()))

let file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype create_decoder

let () =
  Plug.register Decoder.decoders "flac"
    ~doc:
      "Use libflac to decode any file or stream if its MIME type or file \
       extension is appropriate."
    {
      Decoder.priority = (fun () -> priority#get);
      file_extensions = (fun () -> Some file_extensions#get);
      mime_types = (fun () -> Some mime_types#get);
      file_type = (fun ~metadata:_ ~ctype:_ filename -> file_type filename);
      file_decoder = Some file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create_decoder);
    }

let log = Log.make ["metadata"; "flac"]

let get_tags ~metadata:_ ~extension ~mime file =
  if
    not
      (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mime_types#get)
         ~extensions:(Some file_extensions#get) file)
  then raise Not_found;
  let fd = Decoder.openfile file in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd ~write fd in
      match h.Flac.Decoder.File.comments with Some (_, m) -> m | None -> [])

let metadata_decoder_priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "flac")
    "Priority for the flac metadata decoder" ~d:1

let () =
  Plug.register Request.mresolvers "flac" ~doc:""
    {
      Request.priority = (fun () -> metadata_decoder_priority#get);
      resolver = get_tags;
    }

let check filename =
  List.mem (Magic_mime.lookup filename) mime_types#get
  ||
    try
      ignore (file_type filename);
      true
    with _ -> false

let dresolver ~metadata:_ file =
  if not (check file) then raise Not_found;
  let fd = Decoder.openfile file in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let write _ = () in
      let h = Flac.Decoder.File.create_from_fd ~write fd in
      let info = h.Flac.Decoder.File.info in
      match info.Flac.Decoder.total_samples with
        | x when x = Int64.zero -> raise Not_found
        | x -> Int64.to_float x /. float info.Flac.Decoder.sample_rate)

let () =
  Plug.register Request.dresolvers "flac" ~doc:"Compute duration of flac files."
    {
      dpriority = (fun () -> priority#get);
      file_extensions = (fun () -> file_extensions#get);
      dresolver;
    }
