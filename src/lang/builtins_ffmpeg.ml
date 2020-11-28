(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

module InternalResampler =
  Swresample.Make (Swresample.FltPlanarBigArray) (Swresample.Frame)

class consumer ~output_kind ~producer ~kind ~content ~max_buffer ~pre_buffer
  ~source ~format c =
  let codec =
    match format.Ffmpeg_format.audio_codec with
      | Some (`Raw codec) | Some (`Internal codec) ->
          Avcodec.Audio.find_encoder codec
      | _ -> assert false
  in
  let src_channel_layout =
    Avutil.Channel_layout.get_default (Lazy.force Frame.audio_channels)
  in
  let src_sample_rate = Lazy.force Frame.audio_rate in
  let channels = format.Ffmpeg_format.channels in
  let channel_layout = Avutil.Channel_layout.get_default channels in
  let sample_rate = Lazy.force format.Ffmpeg_format.samplerate in
  let time_base = { Avutil.num = 1; den = sample_rate } in
  let get_duration = Ffmpeg_decoder_common.convert_duration ~src:time_base in
  let sample_format = Avcodec.Audio.find_best_sample_format codec `Dbl in
  let encoder =
    Avcodec.Audio.create_encoder ~channel_layout ~channels ~sample_format
      ~sample_rate codec
  in
  let resampler =
    InternalResampler.create ~out_sample_format:sample_format src_channel_layout
      src_sample_rate channel_layout sample_rate
  in
  let s = Lang.to_source source in
  object
    inherit
      Producer_consumer.consumer
        ~output_kind ~producer ~kind ~content ~max_buffer ~pre_buffer ~source c

    method output_send frame =
      let frame = InternalResampler.convert resampler (AFrame.pcm frame) in

      Producer_consumer.(
        proceed c (fun () ->
            if c.abort then (
              c.abort <- false;
              s#abort_track );
            Avcodec.encode encoder
              (fun packet ->
                let duration =
                  get_duration (Avcodec.Packet.get_duration packet)
                in
                let packet = { Ffmpeg_copy_content.packet; time_base } in
                let data =
                  { Ffmpeg_content_base.params = None; data = [(0, packet)] }
                in
                let data = Ffmpeg_copy_content.Audio.lift_data data in
                Generator.put_audio c.generator data 0 duration)
              frame))
  end

let () =
  Lang.add_module "ffmpeg";
  Lang.add_module "ffmpeg.encode"

let () =
  let source_kind = Frame.{ audio = audio_pcm; video = none; midi = none } in
  let source_t = Lang.kind_type_of_kind_format source_kind in
  let return_kind =
    Frame.
      {
        audio = `Kind Ffmpeg_copy_content.Audio.kind;
        video = none;
        midi = none;
      }
  in
  let return_t = Lang.kind_type_of_kind_format return_kind in
  let proto =
    [
      ("", Lang.format_t source_t, None, Some "Encoding format.");
      ("", Lang.source_t source_t, None, None);
      ( "buffer",
        Lang.float_t,
        Some (Lang.float 1.),
        Some "Amount of data to pre-buffer, in seconds." );
      ( "max",
        Lang.float_t,
        Some (Lang.float 10.),
        Some "Maximum amount of buffered data, in seconds." );
    ]
  in
  Lang.add_operator "ffmpeg.encode.audio" proto ~return_t
    ~category:Lang.Conversions ~descr:"Encode a source's content" (fun p ->
      let pre_buffer = Lang.to_float (List.assoc "buffer" p) in
      let max_buffer = Lang.to_float (List.assoc "max" p) in
      let max_buffer = max max_buffer (pre_buffer *. 1.1) in
      let format = Lang.assoc "" 1 p in
      let source = Lang.assoc "" 2 p in
      let format =
        match Lang.to_format format with
          | Encoder.Ffmpeg ffmpeg -> ffmpeg
          | _ ->
              raise
                (Lang_errors.Invalid_value
                   (format, "Only %ffmpeg encoder is currently supported!"))
      in
      let control =
        Producer_consumer.
          {
            generator = Generator.create `Both;
            lock = Mutex.create ();
            buffering = true;
            abort = false;
          }
      in
      let producer =
        new Producer_consumer.producer
          ~kind:return_kind ~name:"ffmpeg.encode" control
      in
      let _ =
        new consumer
          ~producer ~output_kind:"ffmpeg.encode" ~kind:source_kind
          ~content:`Audio ~max_buffer ~pre_buffer ~source ~format control
      in
      producer)
