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

(** Decode and read metadatas of AAC files. *)

let error_translator = function
  | Faad.Error x ->
      Some (Printf.sprintf "Faad error: %s" (Faad.error_message x))
  | _ -> None

let () = Printexc.register_printer error_translator

exception End_of_stream

(** Buffered input device. *)
let buffered_input input =
  let buffer = Strings.Mutable.empty () in
  let pos = ref 0 in
  let drop len =
    pos := !pos + len;
    Strings.Mutable.drop buffer len
  in
  let tell =
    match input.Decoder.tell with
      | None -> None
      | Some f -> Some (fun () -> Strings.Mutable.length buffer + f ())
  in
  let lseek =
    match input.Decoder.lseek with
      | None -> None
      | Some f ->
          let lseek len =
            ignore (Strings.Mutable.flush buffer);
            f len
          in
          Some lseek
  in
  (* Get at most [len] bytes from the buffer, which is refilled from [input] if
     needed. This does not remove data from the buffer. *)
  let tmplen = Utils.buflen in
  let tmp = Bytes.create tmplen in
  let read buf ofs len =
    let size = Strings.Mutable.length buffer in
    let len =
      if size > len then len
      else (
        let read = min tmplen len in
        let read = input.Decoder.read tmp 0 read in
        if read = 0 then raise End_of_stream;
        Strings.Mutable.add_subbytes buffer tmp 0 read;
        min len (size + read))
    in
    Strings.Mutable.blit buffer 0 buf ofs len;
    len
  in
  ({ Decoder.read; tell; lseek; length = None }, drop, pos)

let log = Log.make ["decoder"; "aac"]

let create_decoder input =
  let dec = Faad.create () in
  let input, drop, pos = buffered_input input in
  let offset, samplerate, chans =
    let initbuflen = 1024 in
    let initbuf = Bytes.create initbuflen in
    let len = input.Decoder.read initbuf 0 initbuflen in
    Faad.init dec initbuf 0 len
  in
  drop offset;
  pos := 0;
  let processed = ref 0 in
  let aacbuflen = Faad.min_bytes_per_channel * chans in
  let aacbuf = Bytes.create aacbuflen in
  (* We approximate bitrate for seeking.. *)
  let seek ticks =
    if
      !processed == 0 || !pos == 0
      || input.Decoder.lseek == None
      || input.Decoder.tell == None
    then 0
    else (
      let cur_time = float !processed /. float samplerate in
      let rate = float !pos /. cur_time in
      let offset = Frame.seconds_of_main ticks in
      let bytes = int_of_float (rate *. offset) in
      try
        ignore
          ((Option.get input.Decoder.lseek)
             ((Option.get input.Decoder.tell) () + bytes));
        Faad.post_sync_reset dec;
        ticks
      with _ -> 0)
  in
  {
    Decoder.seek;
    eof = (fun _ -> ());
    close = (fun _ -> ());
    decode =
      (fun buffer ->
        let len = input.Decoder.read aacbuf 0 aacbuflen in
        if len = aacbuflen then (
          let pos, data = Faad.decode dec aacbuf 0 len in
          begin try processed := !processed + Audio.length data with _ -> ()
          end;
          drop pos;

          buffer.Decoder.put_pcm ~samplerate data));
  }

let aac_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "aac")
    "Mime-types used for guessing AAC format"
    ~d:["audio/aac"; "audio/aacp"; "audio/x-hx-aac-adts"]

let aac_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "aac")
    "File extensions used for guessing AAC format" ~d:["aac"]

let aac_priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "aac")
    "Priority for the AAC decoder" ~d:1

(* Get the number of channels of audio in an AAC file. *)
let file_type ~metadata:_ ~ctype:_ filename =
  let fd = Decoder.openfile filename in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let dec = Faad.create () in
      let aacbuflen = Utils.buflen in
      let aacbuf = Bytes.create aacbuflen in
      let _, rate, channels =
        let n = Unix.read fd aacbuf 0 aacbuflen in
        Faad.init dec aacbuf 0 n
      in
      log#important "Libfaad recognizes %s as AAC (%dHz,%d channels)."
        (Lang_string.quote_string filename)
        rate channels;
      Some
        (Frame.Fields.make
           ~audio:(Content.Audio.format_of_channels channels)
           ()))

let file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype create_decoder

let () =
  Plug.register Decoder.decoders "aac"
    ~doc:
      "Use libfaad to decode AAC if MIME type or file extension is appropriate."
    {
      Decoder.priority = (fun () -> aac_priority#get);
      file_extensions = (fun () -> Some aac_file_extensions#get);
      mime_types = (fun () -> Some aac_mime_types#get);
      file_type;
      file_decoder = Some file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create_decoder);
    }

(* Mp4 decoding. *)

let log = Log.make ["decoder"; "mp4"]

exception End_of_track

let create_decoder input =
  let dec = Faad.create () in
  let read = input.Decoder.read in
  let mp4 = Faad.Mp4.openfile ?seek:input.Decoder.lseek read in
  let track = Faad.Mp4.find_aac_track mp4 in
  let samplerate, _ = Faad.Mp4.init mp4 dec track in
  let nb_samples = Faad.Mp4.samples mp4 track in
  let sample = ref 0 in
  let pos = ref 0 in
  let ended = ref false in
  let decode buffer =
    if !ended || !sample >= nb_samples || !sample < 0 then raise End_of_track;
    let data = Faad.Mp4.decode mp4 track !sample dec in
    incr sample;
    begin try pos := !pos + Audio.length data with _ -> ()
    end;
    buffer.Decoder.put_pcm ~samplerate data
  in
  let seek ticks =
    try
      let time = Frame.seconds_of_main ticks in
      let audio_ticks = int_of_float (time *. float samplerate) in
      let offset = max (!pos + audio_ticks) 0 in
      let new_sample, _ = Faad.Mp4.seek mp4 track offset in
      sample := new_sample;
      let time = float (offset - !pos) /. float samplerate in
      pos := offset;
      Frame.main_of_seconds time
    with _ ->
      ended := true;
      0
  in
  { Decoder.decode; seek; eof = (fun _ -> ()); close = (fun _ -> ()) }

(* Get the number of channels of audio in an MP4 file. *)
let file_type ~metadata:_ ~ctype:_ filename =
  let dec = Faad.create () in
  let fd = Decoder.openfile filename in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let mp4 = Faad.Mp4.openfile_fd fd in
      let track = Faad.Mp4.find_aac_track mp4 in
      let rate, channels = Faad.Mp4.init mp4 dec track in
      log#important "Libfaad recognizes %s as MP4 (%dHz,%d channels)."
        (Lang_string.quote_string filename)
        rate channels;
      Some
        (Frame.Fields.make
           ~audio:(Content.Audio.format_of_channels channels)
           ()))

let mp4_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "mp4")
    "Mime-types used for guessing MP4 format"
    ~d:["audio/mp4"; "application/mp4"]

let mp4_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "mp4")
    "File extensions used for guessing MP4 format"
    ~d:["m4a"; "m4b"; "m4p"; "m4v"; "m4r"; "3gp"; "mp4"]

let mp4_priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "mp4")
    "Priority for the MP4 decoder" ~d:1

let file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype create_decoder

let () =
  Plug.register Decoder.decoders "mp4"
    ~doc:
      "Use libfaad to decode MP4 if MIME type or file extension is appropriate."
    {
      Decoder.priority = (fun () -> mp4_priority#get);
      file_extensions = (fun () -> Some mp4_file_extensions#get);
      mime_types = (fun () -> Some mp4_mime_types#get);
      file_type;
      file_decoder = Some file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create_decoder);
    }

let log = Log.make ["metadata"; "mp4"]

let get_tags ~metadata:_ ~extension ~mime file =
  if
    not
      (Decoder.test_file ~log ~extension ~mime ~mimes:(Some mp4_mime_types#get)
         ~extensions:(Some mp4_file_extensions#get) file)
  then raise Not_found;
  let fd = Decoder.openfile file in
  Fun.protect
    ~finally:(fun () -> Unix.close fd)
    (fun () ->
      let mp4 = Faad.Mp4.openfile_fd fd in
      Array.to_list (Faad.Mp4.metadata mp4))

let mp4_metadata_decoder_priority =
  Dtools.Conf.int
    ~p:(Request.conf_metadata_decoder_priorities#plug "mp4")
    "Priority for the mp4 metadata decoder" ~d:1

let () =
  Plug.register Request.mresolvers "mp4" ~doc:"MP4 tag decoder."
    {
      Request.priority = (fun () -> mp4_metadata_decoder_priority#get);
      resolver = get_tags;
    }
