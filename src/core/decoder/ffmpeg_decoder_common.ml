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

(** Decode and read metadata using ffmpeg. *)

let log = Log.make ["decoder"; "ffmpeg"]

let conf_ffmpeg_decoder =
  Dtools.Conf.unit
    ~p:(Decoder.conf_decoder#plug "ffmpeg")
    "FFmpeg decoder configuration"

let conf_codecs =
  Dtools.Conf.unit ~p:(conf_ffmpeg_decoder#plug "codecs") "Codecs settings"

let conf_max_interleave_duration =
  Dtools.Conf.float
    ~p:(conf_ffmpeg_decoder#plug "max_interleave_duration")
    ~d:5. "Maximum data buffered while waiting for all streams."

let conf_max_interleave_delta =
  Dtools.Conf.float
    ~p:(conf_ffmpeg_decoder#plug "max_interleave_delta")
    ~d:0.04 "Maximum delay between interleaved streams."

let conf_codecs =
  let codecs = Hashtbl.create 10 in
  List.iter
    (fun c ->
      let id = Avcodec.Audio.get_id c in
      let codec_name = Avcodec.Audio.string_of_id id in
      let name = Avcodec.Audio.get_name c in
      let c =
        match Hashtbl.find_opt codecs codec_name with
          | Some l -> name :: l
          | None -> [name]
      in
      Hashtbl.replace codecs codec_name c)
    Avcodec.Audio.decoders;

  List.iter
    (fun c ->
      let id = Avcodec.Video.get_id c in
      let codec_name = Avcodec.Video.string_of_id id in
      let name = Avcodec.Video.get_name c in
      let c =
        match Hashtbl.find_opt codecs codec_name with
          | Some l -> name :: l
          | None -> [name]
      in
      Hashtbl.replace codecs codec_name c)
    Avcodec.Video.decoders;

  Hashtbl.fold
    (fun name codecs l ->
      let conf =
        Dtools.Conf.string ~p:(conf_codecs#plug name)
          ("Preferred codec to decode " ^ name)
      in
      ignore
        (Dtools.Conf.list ~p:(conf#plug "available") ~d:codecs
           ("Available codecs to decode " ^ name));
      (name, conf) :: l)
    codecs []

let set_stream_decoder ~get_name ~get_codec stream =
  let params = Av.get_codec_params stream in
  let name = get_name params in
  match List.assoc_opt name conf_codecs with
    | Some conf -> (
        try
          let preferred = conf#get in
          log#info "Trying preferred decoder %s for codec %s" preferred name;
          try Av.set_decoder stream (get_codec preferred)
          with exn ->
            let bt = Printexc.get_backtrace () in
            Utils.log_exception ~log ~bt
              (Printf.sprintf "Failed to set decoder %s for codec %s: %s"
                 preferred name (Printexc.to_string exn))
        with _ -> ())
    | None -> ()

let set_audio_stream_decoder (type a)
    (stream : (Avutil.input, Avutil.audio, a) Av.stream) =
  set_stream_decoder
    ~get_name:(fun p -> Avcodec.Audio.(string_of_id (get_params_id p)))
    ~get_codec:Avcodec.Audio.find_decoder_by_name stream

let set_video_stream_decoder (type a)
    (stream : (Avutil.input, Avutil.video, a) Av.stream) =
  set_stream_decoder
    ~get_name:(fun p -> Avcodec.Video.(string_of_id (get_params_id p)))
    ~get_codec:Avcodec.Video.find_decoder_by_name stream
