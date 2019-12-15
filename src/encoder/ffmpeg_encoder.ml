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

(** FFMPEG encoder *)

open FFmpeg

module Resampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

let log = Ffmpeg_config.log

type handler = {
  output : Avutil.output Avutil.container;
  stream : (Avutil.output, Avutil.audio) Av.stream;
  converter :
    (Swresample.FltPlanarBigArray.t, Swresample.Frame.t) Swresample.ctx;
}

(* Convert ffmpeg-specific options. *)
let convert_options opts =
  let convert name fn =
    match Hashtbl.find_opt opts name with
      | None -> ()
      | Some v -> Hashtbl.replace opts name (fn v)
  in
  convert "sample_fmt" (function
    | `String fmt -> `Int FFmpeg.Avutil.Sample_format.(get_id (find fmt))
    | _ -> assert false);
  convert "channel_layout" (function
    | `String layout -> `Int FFmpeg.Avutil.Channel_layout.(get_id (find layout))
    | _ -> assert false)

let encoder ffmpeg meta =
  let short_name = ffmpeg.Ffmpeg_format.format in
  let format =
    match Av.Format.guess_output_format ~short_name () with
      | None -> failwith "No format for filename!"
      | Some f -> f
  in
  let codec = Avcodec.Audio.find_encoder ffmpeg.Ffmpeg_format.codec in
  let out_sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in
  let src_freq = Frame.audio_of_seconds 1. in
  let channels = Lazy.force Frame.audio_channels in
  let src_channels =
    match channels with
      | 1 -> `Mono
      | 2 -> `Stereo
      | _ ->
          failwith "%ffmpeg encoder only supports mono or stereo audio for now!"
  in
  let dst_freq = Lazy.force ffmpeg.Ffmpeg_format.samplerate in
  let dst_channels =
    match ffmpeg.Ffmpeg_format.channels with
      | 1 -> `Mono
      | 2 -> `Stereo
      | _ ->
          failwith "%ffmpeg encoder only supports mono or stereo audio for now!"
  in
  let buf = Strings.Mutable.empty () in
  let options = Hashtbl.copy ffmpeg.Ffmpeg_format.options in
  convert_options options;
  let make () =
    let opts =
      Av.mk_audio_opts ~channels:ffmpeg.Ffmpeg_format.channels
        ~sample_rate:(Lazy.force ffmpeg.Ffmpeg_format.samplerate)
        ()
    in
    Hashtbl.iter (Hashtbl.add opts) options;
    let converter =
      Resampler.create ~out_sample_format src_channels src_freq dst_channels
        dst_freq
    in
    let write str ofs len =
      Strings.Mutable.add_subbytes buf str ofs len;
      len
    in
    let output = Av.open_output_stream ~opts write format in
    let stream = Av.new_audio_stream ~opts ~codec output in
    if Hashtbl.length opts > 0 then
      failwith
        (Printf.sprintf "Unrecognized options: %s"
           (Ffmpeg_format.string_of_options opts));
    { output; stream; converter }
  in
  let h = ref (make ()) in
  let encode frame start len =
    let start = Frame.audio_of_master start in
    let len = Frame.audio_of_master len in
    let data =
      Audio.sub (AFrame.content_of_type ~channels frame start) start len
    in
    let frame = Resampler.convert !h.converter data in
    Av.write_frame !h.stream frame;
    Strings.Mutable.flush buf
  in
  let insert_metadata m =
    Av.close !h.output;
    h := make ();
    let m =
      Hashtbl.fold (fun lbl v l -> (lbl, v) :: l) (Meta_format.to_metadata m) []
    in
    Av.set_metadata !h.stream m
  in
  insert_metadata meta;
  let stop () =
    Av.close !h.output;
    Strings.Mutable.flush buf
  in
  { Encoder.insert_metadata; header = Strings.empty; encode; stop }

let () =
  Encoder.plug#register "FFMPEG" (function
    | Encoder.Ffmpeg m -> Some (fun _ -> encoder m)
    | _ -> None)
