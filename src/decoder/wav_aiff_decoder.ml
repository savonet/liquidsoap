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

(** Decode WAV files. *)

let log = Log.make ["decoder"; "wav/aiff"]

(** {1 Generic decoder} *)

exception End_of_stream

let really_input input buf ofs len =
  let rec f pos =
    if pos < len then (
      let i = input buf (ofs + pos) (len - pos) in
      if i = 0 then raise End_of_stream else f (pos + i) )
  in
  f ofs

let input fn = fn

let input_byte input =
  let buf = Bytes.create 1 in
  let i = input buf 0 1 in
  if i = 0 then raise End_of_stream ;
  int_of_char (Bytes.get buf 0)

let seek input len =
  let s = Bytes.create len in
  ignore (really_input input s 0 len)

let input_ops =
  {Wav_aiff.really_input; input_byte; input; seek; close= (fun _ -> ())}

module Make (Generator : Generator.S_Asio) = struct
  (* It might be more efficient to write our code for an input
   * channel and use directly the one we have when decoding files
   * or external processes, if we could wrap the input function used
   * for decoding stream (in http and harbor) as an in_channel. *)
  let create ?header input =
    let decoder = ref (fun _ -> assert false) in
    let header = ref header in
    let main_decoder remaining =
      let remaining = ref remaining in
      let bytes_to_get = Utils.pagesize * 64 in
      let buf = Bytes.create bytes_to_get in
      fun converter gen ->
        let bytes_to_get =
          if !remaining = -1 then bytes_to_get else min !remaining bytes_to_get
        in
        let bytes = input.Decoder.read buf 0 bytes_to_get in
        if !remaining <> -1 then remaining := !remaining - bytes ;
        if bytes = 0 then raise End_of_stream ;
        let content = converter (Bytes.sub_string buf 0 bytes) in
        Generator.set_mode gen `Audio ;
        Generator.put_audio gen content 0 (Audio.Mono.length content.(0))
    in
    let read_header () =
      let iff_header = Wav_aiff.read_header input_ops input.Decoder.read in
      let samplesize = Wav_aiff.sample_size iff_header in
      let channels = Wav_aiff.channels iff_header in
      let samplerate = Wav_aiff.sample_rate iff_header in
      let datalen = Wav_aiff.data_length iff_header in
      let datalen = if datalen <= 0 then -1 else datalen in
      let format = Wav_aiff.format_of_handler iff_header in
      let converter =
        Rutils.create_from_iff ~samplesize ~channels ~format
          ~audio_src_rate:(float samplerate)
      in
      let format_descr = match format with `Wav -> "WAV" | `Aiff -> "AIFF" in
      log#info
        "%s header read (%d Hz, %d bits, %d bytes), starting decoding..."
        format_descr samplerate samplesize datalen ;
      header := Some (format, samplesize, channels, float samplerate, datalen) ;
      decoder := main_decoder datalen converter
    in
    begin
      match !header with None -> decoder := fun _ -> read_header ()
      | Some (format, samplesize, channels, audio_src_rate, datalen) ->
          let converter =
            Rutils.create_from_iff ~format ~samplesize ~channels
              ~audio_src_rate
          in
          decoder := main_decoder datalen converter
    end ;
    let seek ticks =
      match (input.Decoder.lseek, input.Decoder.tell, !header) with
        | Some seek, Some tell, Some (_, samplesize, channels, samplerate, _)
          -> (
            (* seek is in absolute position *)
            let duration = Frame.seconds_of_master ticks in
            let samples = int_of_float (duration *. samplerate) in
            let bytes = samples * samplesize * channels / 8 in
            try
              let pos = tell () in
              let ret = seek (pos + bytes) in
              let samples = 8 * (ret - pos) / (samplesize * channels) in
              let duration = float samples /. samplerate in
              Frame.master_of_seconds duration
            with _ -> 0 )
        | _, _, _ ->
            0
    in
    {Decoder.decode= (fun gen -> !decoder gen); seek}
end

module Generator_plus = Generator.From_audio_video_plus
module Generator = Generator.From_audio_video
module Buffered = Decoder.Buffered (Generator)

(* File decoding *)

module D = Make (Generator)

let get_type filename =
  let header = Wav_aiff.fopen filename in
  Tutils.finalize
    ~k:(fun () -> Wav_aiff.close header)
    (fun () ->
      let channels =
        let channels = Wav_aiff.channels header in
        let sample_rate = Wav_aiff.sample_rate header in
        let ok_message s =
          log#info "%S recognized as WAV file (%s,%dHz,%d channels)." filename
            s sample_rate channels
        in
        match Wav_aiff.sample_size header with
          | 8 ->
              ok_message "u8" ; channels
          | 16 ->
              ok_message "s16le" ; channels
          | 24 ->
              ok_message "s24le" ; channels
          | 32 ->
              ok_message "s32le" ; channels
          | _ ->
              log#info
                "Only 8, 16, 24 and 32 bit WAV files are supported at the \
                 moment.." ;
              0
      in
      {Frame.video= 0; midi= 0; audio= channels})

let create_file_decoder filename kind =
  let generator = Generator.create `Audio in
  Buffered.file_decoder filename kind (D.create ?header:None) generator

let wav_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "wav")
    "Mime-types used for guessing WAV format"
    ~d:["audio/vnd.wave"; "audio/wav"; "audio/wave"; "audio/x-wav"]

let wav_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "wav")
    "File extensions used for guessing WAV format" ~d:["wav"; "wave"]

let () =
  Decoder.file_decoders#register "WAV"
    ~sdoc:"Decode as WAV any file with a correct header."
    (fun ~metadata:_ filename kind ->
      (* Don't get the file's type if no audio is allowed anyway. *)
      if
        kind.Frame.audio = Frame.Zero
        || not
             (Decoder.test_file ~mimes:wav_mime_types#get
                ~extensions:wav_file_extensions#get ~log filename)
      then None
      else (
        let file_type = get_type filename in
        if Frame.type_has_kind file_type kind then
          Some (fun () -> create_file_decoder filename kind)
        else (
          log#important "WAV file %S has content type %s but %s was expected."
            filename
            (Frame.string_of_content_type file_type)
            (Frame.string_of_content_kind kind) ;
          None ) ))

let aiff_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "aiff")
    "Mime-types used for guessing AIFF format"
    ~d:["audio/x-aiff"; "audio/aiff"]

let aiff_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "aiff")
    "File extensions used for guessing AIFF format" ~d:["aiff"; "aif"; "aifc"]

let () =
  Decoder.file_decoders#register "AIFF"
    ~sdoc:"Decode as AIFF any file with a correct header."
    (fun ~metadata:_ filename kind ->
      (* Don't get the file's type if no audio is allowed anyway. *)
      if
        kind.Frame.audio = Frame.Zero
        || not
             (Decoder.test_file ~mimes:aiff_mime_types#get
                ~extensions:aiff_file_extensions#get ~log filename)
      then None
      else (
        let file_type = get_type filename in
        if Frame.type_has_kind file_type kind then
          Some (fun () -> create_file_decoder filename kind)
        else (
          log#important "AIFF file %S has content type %s but %s was expected."
            filename
            (Frame.string_of_content_type file_type)
            (Frame.string_of_content_kind kind) ;
          None ) ))

let () =
  let duration file =
    let w = Wav_aiff.fopen file in
    let ret = Wav_aiff.duration w in
    Wav_aiff.close w ; ret
  in
  Request.dresolvers#register "WAV/AIFF" duration

(* Stream decoding *)

module D_stream = Make (Generator_plus)

let () =
  Decoder.stream_decoders#register "WAV"
    ~sdoc:"Decode a WAV stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime wav_mime_types#get
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
        Some (D_stream.create ?header:None)
      else None)

let () =
  Decoder.stream_decoders#register "AIFF"
    ~sdoc:"Decode a AIFF stream with an appropriate MIME type."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime aiff_mime_types#get
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
        Some (D_stream.create ?header:None)
      else None)

let mime_types_basic =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "basic")
    "Mime-types used for guessing PCM/BASIC format" ~d:["audio/basic"]

let () =
  Decoder.stream_decoders#register "PCM/BASIC"
    ~sdoc:"Decode audio/basic as headerless stereo U8 PCM at 8kHz."
    (fun mime kind ->
      let ( <: ) a b = Frame.mul_sub_mul a b in
      if
        List.mem mime mime_types_basic#get
        (* Check that it is okay to have zero video and midi,
         * and two audio channels. *)
        && Frame.Zero <: kind.Frame.video
        && Frame.Zero <: kind.Frame.midi
        && Frame.Succ (Frame.Succ Frame.Zero) <: kind.Frame.audio
      then Some (D_stream.create ~header:(`Wav, 8, 2, 8000., -1))
      else None)
