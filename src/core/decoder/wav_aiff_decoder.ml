(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2024 Savonet team

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
      if i = 0 then raise End_of_stream else f (pos + i))
  in
  f ofs

let input fn = fn

let input_byte input =
  let buf = Bytes.create 1 in
  let i = input buf 0 1 in
  if i = 0 then raise End_of_stream;
  int_of_char (Bytes.get buf 0)

let seek input len =
  let s = Bytes.create len in
  ignore (really_input input s 0 len)

let input_ops =
  { Wav_aiff.really_input; input_byte; input; seek; close = (fun _ -> ()) }

(* It might be more efficient to write our code for an input
 * channel and use directly the one we have when decoding files
 * or external processes, if we could wrap the input function used
 * for decoding stream (in http and harbor) as an in_channel. *)
let create ?header input =
  let decoder = ref (fun ~buffer:_ -> assert false) in
  let header = ref header in
  let main_decoder ~samplerate ~buffer remaining =
    let remaining = ref remaining in
    let bytes_to_get = Utils.pagesize * 64 in
    let buf = Bytes.create bytes_to_get in
    fun converter ->
      let bytes_to_get =
        if !remaining = -1 then bytes_to_get else min !remaining bytes_to_get
      in
      let bytes = input.Decoder.read buf 0 bytes_to_get in
      if !remaining <> -1 then remaining := !remaining - bytes;
      if bytes = 0 then raise End_of_stream;
      let content = converter buf 0 bytes in
      buffer.Decoder.put_pcm ~samplerate content
  in
  let read_header () =
    let iff_header = Wav_aiff.read_header input_ops input.Decoder.read in
    let samplesize = Wav_aiff.sample_size iff_header in
    let channels = Wav_aiff.channels iff_header in
    let samplerate = Wav_aiff.sample_rate iff_header in
    let datalen = Wav_aiff.data_length iff_header in
    let datalen = if datalen <= 0 then -1 else datalen in
    let format = Wav_aiff.format_of_handler iff_header in
    let converter = Decoder_utils.from_iff ~samplesize ~channels ~format in
    let format_descr = match format with `Wav -> "WAV" | `Aiff -> "AIFF" in
    log#info "%s header read (%d Hz, %d bits, %d bytes), starting decoding..."
      format_descr samplerate samplesize datalen;
    header := Some (format, samplesize, channels, samplerate, datalen);
    decoder := main_decoder ~samplerate datalen converter
  in
  begin
    match !header with
      | None -> decoder := fun ~buffer:_ -> read_header ()
      | Some (format, samplesize, channels, samplerate, datalen) ->
          let converter =
            Decoder_utils.from_iff ~format ~samplesize ~channels
          in
          decoder := main_decoder ~samplerate datalen converter
  end;
  let seek ticks =
    match (input.Decoder.lseek, input.Decoder.tell, !header) with
      | Some seek, Some tell, Some (_, samplesize, channels, samplerate, _) -> (
          (* seek is in absolute position *)
          let duration = Frame.seconds_of_main ticks in
          let samples = int_of_float (duration *. float samplerate) in
          let bytes = samples * samplesize * channels / 8 in
          try
            let pos = tell () in
            let ret = seek (pos + bytes) in
            let samples = 8 * (ret - pos) / (samplesize * channels) in
            let duration = float samples /. float samplerate in
            Frame.main_of_seconds duration
          with _ -> 0)
      | _, _, _ -> 0
  in
  {
    Decoder.decode = (fun buffer -> !decoder ~buffer);
    seek;
    eof = (fun _ -> ());
    close = (fun _ -> ());
  }

(* File decoding *)

let file_type ~metadata:_ ~ctype:_ filename =
  let header = Wav_aiff.fopen filename in
  Fun.protect
    ~finally:(fun () -> Wav_aiff.close header)
    (fun () ->
      let channels =
        let channels = Wav_aiff.channels header in
        let sample_rate = Wav_aiff.sample_rate header in
        let ok_message s =
          log#info "%s recognized as WAV file (%s,%dHz,%d channels)."
            (Lang_string.quote_string filename)
            s sample_rate channels
        in
        match Wav_aiff.sample_size header with
          | 8 ->
              ok_message "u8";
              channels
          | 16 ->
              ok_message "s16le";
              channels
          | 24 ->
              ok_message "s24le";
              channels
          | 32 ->
              ok_message "s32le";
              channels
          | _ ->
              log#info
                "Only 8, 16, 24 and 32 bit WAV files are supported at the \
                 moment..";
              0
      in
      Some
        (Frame.Fields.make
           ~audio:
             (Frame_base.format_of_channels ~pcm_kind:Content.Audio.kind
                channels)
           ()))

let wav_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "wav")
    "Mime-types used for guessing WAV format"
    ~d:["audio/vnd.wave"; "audio/wav"; "audio/wave"; "audio/x-wav"]

let wav_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "wav")
    "File extensions used for guessing WAV format" ~d:["wav"; "wave"]

let wav_priority =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "wav")
    "Priority for the WAV decoder" ~d:1

let create_file_decoder ~metadata:_ ~ctype filename =
  Decoder.opaque_file_decoder ~filename ~ctype (create ?header:None)

let () =
  Plug.register Decoder.decoders "wav" ~doc:"Decode file or streams as WAV."
    {
      Decoder.priority = (fun () -> wav_priority#get);
      file_extensions = (fun () -> Some wav_file_extensions#get);
      mime_types = (fun () -> Some wav_mime_types#get);
      file_type;
      file_decoder = Some create_file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create ?header:None);
    }

let aiff_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "aiff")
    "Mime-types used for guessing AIFF format"
    ~d:["audio/x-aiff"; "audio/aiff"]

let aiff_file_extensions =
  Dtools.Conf.list
    ~p:(Decoder.conf_file_extensions#plug "aiff")
    "File extensions used for guessing AIFF format" ~d:["aiff"; "aif"; "aifc"]

let aiff_priorities =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "aiff")
    "Priority for the AIFF decoder" ~d:1

let () =
  Plug.register Decoder.decoders "aiff"
    ~doc:"Decode as AIFF any file with a correct header."
    {
      Decoder.priority = (fun () -> aiff_priorities#get);
      file_extensions = (fun () -> Some aiff_file_extensions#get);
      mime_types = (fun () -> Some aiff_mime_types#get);
      file_type;
      file_decoder = Some create_file_decoder;
      stream_decoder = Some (fun ~ctype:_ _ -> create ?header:None);
    }

let () =
  let dresolver ~metadata:_ file =
    let w = Wav_aiff.fopen file in
    let ret = Wav_aiff.duration w in
    Wav_aiff.close w;
    ret
  in
  Plug.register Request.dresolvers "wav/aiff"
    ~doc:"Native computation of wav and aiff files duration."
    {
      dpriority = (fun () -> aiff_priorities#get);
      file_extensions = (fun () -> aiff_file_extensions#get);
      dresolver;
    }

let basic_mime_types =
  Dtools.Conf.list
    ~p:(Decoder.conf_mime_types#plug "basic")
    "Mime-types used for guessing PCM/BASIC format" ~d:["audio/basic"]

let basic_priorities =
  Dtools.Conf.int
    ~p:(Decoder.conf_priorities#plug "basic")
    "Priority for the PCM/BASIC decoder" ~d:1

let () =
  Plug.register Decoder.decoders "pcm/basic"
    ~doc:"Decode audio/basic as headerless stereo U8 PCM at 8kHz."
    {
      Decoder.priority = (fun () -> basic_priorities#get);
      file_extensions = (fun () -> None);
      mime_types = (fun () -> Some basic_mime_types#get);
      file_type = (fun ~metadata:_ ~ctype:_ _ -> None);
      file_decoder = None;
      stream_decoder =
        Some (fun ~ctype:_ _ -> create ~header:(`Wav, 8, 2, 8000, -1));
    }
