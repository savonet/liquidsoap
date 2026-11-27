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

exception Invalid_settings of string

let create speex ~metadata () =
  let frames_per_packet = speex.Speex_format.frames_per_packet in
  let mode =
    match speex.Speex_format.mode with
      | Speex_format.Narrowband -> Speex.Narrowband
      | Speex_format.Wideband -> Speex.Wideband
      | Speex_format.Ultra_wideband -> Speex.Ultra_wideband
  in
  let vbr =
    match speex.Speex_format.bitrate_control with
      | Speex_format.Vbr _ -> true
      | _ -> false
  in
  let channels = if speex.Speex_format.stereo then 2 else 1 in
  let rate = Deferred.force speex.Speex_format.samplerate in
  let header =
    Speex.Header.init ~frames_per_packet ~mode ~vbr ~nb_channels:channels ~rate
      ()
  in
  let enc = Speex.Encoder.init mode frames_per_packet in
  begin match speex.Speex_format.bitrate_control with
    | Speex_format.Vbr x ->
        Speex.Encoder.set enc Speex.SPEEX_SET_VBR 1;
        Speex.Encoder.set enc Speex.SPEEX_SET_VBR_QUALITY x
    | Speex_format.Abr x ->
        Speex.Encoder.set enc Speex.SPEEX_SET_ABR 1;
        Speex.Encoder.set enc Speex.SPEEX_SET_BITRATE x
    | Speex_format.Quality x -> Speex.Encoder.set enc Speex.SPEEX_SET_QUALITY x
  end;
  begin match speex.Speex_format.complexity with
    | Some complexity ->
        Speex.Encoder.set enc Speex.SPEEX_SET_COMPLEXITY complexity
    | _ -> ()
  end;
  if speex.Speex_format.dtx then Speex.Encoder.set enc Speex.SPEEX_SET_DTX 1;
  if speex.Speex_format.vad then Speex.Encoder.set enc Speex.SPEEX_SET_VAD 1;
  Speex.Encoder.set enc Speex.SPEEX_SET_SAMPLING_RATE rate;
  let frame_size = Speex.Encoder.get enc Speex.SPEEX_GET_FRAME_SIZE in
  let p1, p2 = Speex.Header.encode_header_packetout header metadata in
  let header_encoder os =
    Ogg.Stream.put_packet os p1;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os =
    let serialno = Ogg.Stream.serialno os in
    Some (Speex.Skeleton.fisbone ~header ~serialno ())
  in
  let stream_start os =
    Ogg.Stream.put_packet os p2;
    Ogg_muxer.flush_pages os
  in
  let packet_length = frame_size * frames_per_packet in
  let to_int x = max (-32768) (min 32767 (int_of_float (32767. *. x))) in
  (* Between calls, 1 to [packet_length] samples are pending so that the
     stream's end always has a last packet to flag. *)
  let pending = ref (Array.make channels [||]) in
  let input_samples = ref 0 in
  let encode_packet ?eos ~granulepos os samples =
    Speex.Encoder.encode_packet_int ?eos ~granulepos enc os
      (Array.map (Array.map to_int) samples)
  in
  let data_encoder data os _ =
    let { Ogg_muxer.data; offset; length } = data in
    input_samples := !input_samples + length;
    let buf =
      Array.mapi
        (fun c pending ->
          Array.append pending (Array.sub data.(c) offset length))
        !pending
    in
    let available = Array.length buf.(0) in
    let packets = (available - 1) / packet_length in
    for n = 0 to packets - 1 do
      let consumed = (n + 1) * packet_length in
      encode_packet
        ~granulepos:(Int64.of_int (!input_samples - available + consumed))
        os
        (Array.map (fun x -> Array.sub x (n * packet_length) packet_length) buf)
    done;
    let consumed = packets * packet_length in
    pending :=
      Array.map (fun x -> Array.sub x consumed (available - consumed)) buf
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else Ogg_muxer.Time (Int64.to_float granulepos /. float rate)
  in
  let end_of_stream os =
    let samples =
      Array.map
        (fun x ->
          Array.append x (Array.make (packet_length - Array.length x) 0.))
        !pending
    in
    encode_packet ~eos:true ~granulepos:(Int64.of_int !input_samples) os samples;
    Ogg_muxer.flush_pages os
  in
  {
    Ogg_muxer.header_encoder;
    fisbone_packet;
    stream_start;
    data_encoder = Ogg_muxer.Audio_encoder data_encoder;
    end_of_page;
    end_of_stream;
  }

let create_speex = function
  | Ogg_format.Speex speex ->
      let reset ogg_enc m =
        let metadata = Ogg_encoder.comments m in
        let enc = create speex ~metadata () in
        Ogg_muxer.register_track ?fill:speex.Speex_format.fill ogg_enc enc
      in
      let channels = if speex.Speex_format.stereo then 2 else 1 in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float (Deferred.force speex.Speex_format.samplerate) in
      let encode, drain =
        Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq ()
      in
      { Ogg_encoder.reset; encode; drain; id = None }
  | _ -> assert false

let () = Hashtbl.replace Ogg_encoder.audio_encoders "speex" create_speex
