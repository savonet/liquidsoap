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
  let tmplen = Utils.pagesize in
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
        min len (size + read) )
    in
    Strings.Mutable.blit buffer 0 buf ofs len;
    len
  in
  ({ Decoder.read; tell; lseek; length = None }, drop, pos)

let log = Log.make ["decoder"; "aac"]

module Make (Generator : Generator.S_Asio) = struct
  let create_decoder input =
    let resampler = Rutils.create_audio () in
    let dec = Faad.create () in
    let input, drop, pos = buffered_input input in
    let offset, sample_freq, chans =
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
        let cur_time = float !processed /. float sample_freq in
        let rate = float !pos /. cur_time in
        let offset = Frame.seconds_of_master ticks in
        let bytes = int_of_float (rate *. offset) in
        try
          ignore
            ((Utils.get_some input.Decoder.lseek)
               ((Utils.get_some input.Decoder.tell) () + bytes));
          Faad.post_sync_reset dec;
          ticks
        with _ -> 0 )
    in
    {
      Decoder.seek;
      decode =
        (fun gen ->
          let len = input.Decoder.read aacbuf 0 aacbuflen in
          if len = aacbuflen then (
            let pos, data = Faad.decode dec aacbuf 0 len in
            let data = Audio.of_array data in
            begin
              try processed := !processed + Audio.length data with _ -> ()
            end;
            drop pos;
            let content = resampler ~audio_src_rate:(float sample_freq) data in
            (* TODO assert (Array.length content.(0) = length) ? *)
            Generator.set_mode gen `Audio;
            Generator.put_audio gen content 0 (Audio.length content) ));
    }
end

let aac_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "aac")
    "Mime-types used for guessing AAC format"
    ~d:["audio/aac"; "audio/aacp"; "audio/x-hx-aac-adts"]

let aac_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "aac")
    "File extensions used for guessing AAC format" ~d:["aac"]

module G = Generator.From_audio_video
module Buffered = Decoder.Buffered (G)
module Aac = Make (G)

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  Buffered.file_decoder filename kind Aac.create_decoder generator

(* Get the number of channels of audio in an AAC file. *)
let get_type filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let dec = Faad.create () in
      let aacbuflen = Utils.pagesize in
      let aacbuf = Bytes.create aacbuflen in
      let _, rate, channels =
        let n = Unix.read fd aacbuf 0 aacbuflen in
        Faad.init dec aacbuf 0 n
      in
      log#info "Libfaad recognizes %S as AAC (%dHz,%d channels)." filename rate
        channels;
      { Frame.audio = channels; video = 0; midi = 0 })

let () =
  Decoder.file_decoders#register "AAC"
    ~sdoc:
      "Use libfaad to decode AAC if MIME type or file extension is appropriate."
    (fun ~metadata:_ filename kind ->
      (* Before doing anything, check that we are allowed to produce
       * audio, and don't have to produce midi or video. Only then
       * check that the file seems relevant for AAC decoding. *)
      let content = get_type filename in
      if
        content.Frame.audio = 0
        || (not
              ( Frame.mul_sub_mul Frame.Zero kind.Frame.video
              && Frame.mul_sub_mul Frame.Zero kind.Frame.midi ))
        || not
             (Decoder.test_file ~mimes:aac_mime_types#get
                ~extensions:aac_file_extensions#get ~log filename)
      then None
      else if
        kind.Frame.audio = Frame.Variable
        || kind.Frame.audio = Frame.Succ Frame.Variable
        || Frame.type_has_kind content kind
      then Some (fun () -> create_file_decoder filename kind)
      else None)

module D_stream = Make (Generator.From_audio_video_plus)

let () =
  Decoder.stream_decoders#register "AAC"
    ~sdoc:"Use libfaad to decode any stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime aac_mime_types#get
        (* Check that it is okay to have zero video and midi,
         * and at least one audio channel. *)
        && Frame.Zero <: kind.Frame.video
        && Frame.Zero <: kind.Frame.midi
        && kind.Frame.audio <> Frame.Zero
      then
        (* In fact we can't be sure that we'll satisfy the content
         * kind, because the stream might be mono or stereo.
         * For now, we let this problem result in an error at
         * decoding-time. Failing early would only be an advantage
         * if there was possibly another plugin for decoding
         * correctly the stream (e.g. by performing conversions). *)
        Some D_stream.create_decoder
      else None)

(* Mp4 decoding. *)

let log = Log.make ["decoder"; "mp4"]

module Make_mp4 (Generator : Generator.S_Asio) = struct
  exception End_of_track

  let create_decoder input =
    let dec = Faad.create () in
    let read = input.Decoder.read in
    let mp4 = Faad.Mp4.openfile ?seek:input.Decoder.lseek read in
    let resampler = Rutils.create_audio () in
    let track = Faad.Mp4.find_aac_track mp4 in
    let sample_freq, _ = Faad.Mp4.init mp4 dec track in
    let nb_samples = Faad.Mp4.samples mp4 track in
    let sample = ref 0 in
    let pos = ref 0 in
    let ended = ref false in
    let decode gen =
      if !ended || !sample >= nb_samples || !sample < 0 then raise End_of_track;
      let data = Faad.Mp4.decode mp4 track !sample dec in
      let data = Audio.of_array data in
      incr sample;
      begin
        try pos := !pos + Audio.length data with _ -> ()
      end;
      let content = resampler ~audio_src_rate:(float sample_freq) data in
      Generator.set_mode gen `Audio;
      Generator.put_audio gen content 0 (Audio.length content)
    in
    let seek ticks =
      try
        let time = Frame.seconds_of_master ticks in
        let audio_ticks = int_of_float (time *. float sample_freq) in
        let offset = max (!pos + audio_ticks) 0 in
        let new_sample, _ = Faad.Mp4.seek mp4 track offset in
        sample := new_sample;
        let time = float (offset - !pos) /. float sample_freq in
        pos := offset;
        Frame.master_of_seconds time
      with _ ->
        ended := true;
        0
    in
    { Decoder.decode; seek }
end

(* Get the number of channels of audio in an MP4 file. *)
let get_type filename =
  let dec = Faad.create () in
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o644 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let mp4 = Faad.Mp4.openfile_fd fd in
      let track = Faad.Mp4.find_aac_track mp4 in
      let rate, channels = Faad.Mp4.init mp4 dec track in
      log#info "Libfaad recognizes %S as MP4 (%dHz,%d channels)." filename rate
        channels;
      { Frame.audio = channels; video = 0; midi = 0 })

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

module Mp4_dec = Make_mp4 (G)

let create_file_decoder filename kind =
  let generator = G.create `Audio in
  Buffered.file_decoder filename kind Mp4_dec.create_decoder generator

let () =
  Decoder.file_decoders#register "MP4"
    ~sdoc:
      "Use libfaad to decode MP4 if MIME type or file extension is appropriate."
    (fun ~metadata:_ filename kind ->
      (* Before doing anything, check that we are allowed to produce
       * audio, and don't have to produce midi or video. Only then
       * check that the file seems relevant for MP4 decoding. *)
      let content = get_type filename in
      if
        content.Frame.audio = 0
        || kind.Frame.audio = Frame.Zero
        || (not
              ( Frame.mul_sub_mul Frame.Zero kind.Frame.video
              && Frame.mul_sub_mul Frame.Zero kind.Frame.midi ))
        || not
             (Decoder.test_file ~mimes:mp4_mime_types#get
                ~extensions:mp4_file_extensions#get ~log filename)
      then None
      else if
        kind.Frame.audio = Frame.Variable
        || kind.Frame.audio = Frame.Succ Frame.Variable
        || Frame.type_has_kind content kind
      then Some (fun () -> create_file_decoder filename kind)
      else None)

let log = Log.make ["metadata"; "mp4"]

let get_tags file =
  if
    not
      (Decoder.test_file ~mimes:mp4_mime_types#get
         ~extensions:mp4_file_extensions#get ~log file)
  then raise Not_found;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun () ->
      let mp4 = Faad.Mp4.openfile_fd fd in
      Array.to_list (Faad.Mp4.metadata mp4))

let () = Request.mresolvers#register "MP4" get_tags
