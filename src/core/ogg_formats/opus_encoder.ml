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

let create_encoder ~opus ~comments () =
  let samplerate = opus.Opus_format.samplerate in
  let channels = opus.Opus_format.channels in
  let frame_size =
    int_of_float (opus.Opus_format.frame_size *. float samplerate /. 1000.)
  in
  let frame_size_in_main = Frame.main_of_audio frame_size in
  let gen =
    Generator.create
      (Frame.Fields.make ~audio:(Content.Audio.format_of_channels channels) ())
  in
  let application =
    match opus.Opus_format.application with None -> `Audio | Some a -> a
  in
  let enc = ref None in
  let started = ref false in
  let get_enc os =
    match !enc with
      | Some x -> x
      | None ->
          let x =
            Opus.Encoder.create ~comments ~channels ~samplerate ~application os
          in
          Opus.Encoder.apply_control (`Set_bitrate opus.Opus_format.bitrate) x;
          begin
            match opus.Opus_format.mode with
              | Opus_format.CBR -> Opus.Encoder.apply_control (`Set_vbr false) x
              | Opus_format.VBR b ->
                  Opus.Encoder.apply_control (`Set_vbr true) x;
                  Opus.Encoder.apply_control (`Set_vbr_constraint b) x
          end;
          let maybe name value =
            ignore
              (Option.map
                 (fun value -> Opus.Encoder.apply_control (name value) x)
                 value)
          in
          maybe (fun v -> `Set_complexity v) opus.Opus_format.complexity;
          maybe (fun v -> `Set_max_bandwidth v) opus.Opus_format.max_bandwidth;
          maybe (fun v -> `Set_signal v) opus.Opus_format.signal;
          Opus.Encoder.apply_control (`Set_dtx opus.Opus_format.dtx) x;
          Opus.Encoder.apply_control
            (`Set_phase_inversion_disabled
               (not opus.Opus_format.phase_inversion)) x;
          enc := Some x;
          x
  in
  let header_encoder os =
    let enc = get_enc os in
    Ogg.Stream.put_packet os (Opus.Encoder.header enc);
    Ogg.Stream.flush_page os
  in
  let fisbone_packet _ = None in
  let stream_start os =
    let enc = get_enc os in
    Ogg.Stream.put_packet os (Opus.Encoder.comments enc);
    Ogg_muxer.flush_pages os
  in
  let data_encoder { Ogg_muxer.data; offset; length } os _ =
    started := true;
    let enc = get_enc os in
    let content = Content.Audio.lift_data data in
    let offset = Frame.main_of_audio offset in
    let length = Frame.main_of_audio length in
    Generator.put gen Frame.Fields.audio (Content.sub content offset length);
    while Generator.length gen >= frame_size_in_main do
      let content =
        Frame.Fields.find Frame.Fields.audio
          (Generator.slice gen frame_size_in_main)
      in
      let pcm = Content.Audio.get_data content in
      let ret =
        Opus.Encoder.encode_float ~frame_size:opus.Opus_format.frame_size enc
          pcm 0 frame_size
      in
      assert (ret = frame_size)
    done
  in
  let empty_data () =
    {
      Ogg_muxer.offset = 0;
      length = 1;
      data = Array.make (Lazy.force Frame.audio_channels) (Array.make 1 0.);
    }
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else Ogg_muxer.Time (Int64.to_float granulepos /. 48000.)
  in
  let end_of_stream os =
    (* Assert that at least some data was encoded.. *)
    if not !started then data_encoder (empty_data ()) os ();
    Ogg.Stream.terminate os
  in
  {
    Ogg_muxer.header_encoder;
    fisbone_packet;
    stream_start;
    data_encoder = Ogg_muxer.Audio_encoder data_encoder;
    end_of_page;
    end_of_stream;
  }

let create_opus = function
  | Ogg_format.Opus opus ->
      let reset ogg_enc m =
        let comments =
          Frame.Metadata.to_list (Frame.Metadata.Export.to_metadata m)
        in
        let enc = create_encoder ~opus ~comments () in
        Ogg_muxer.register_track ?fill:opus.Opus_format.fill ogg_enc enc
      in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float opus.Opus_format.samplerate in
      let channels = opus.Opus_format.channels in
      let encode = Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq () in
      { Ogg_encoder.encode; reset; id = None }
  | _ -> assert false

let () = Hashtbl.replace Ogg_encoder.audio_encoders "opus" create_opus
