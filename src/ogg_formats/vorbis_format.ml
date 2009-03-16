(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

module Generator = Float_pcm.Generator

let check = Vorbis.Decoder.check_packet

let buflen = 1024

let decoder os =
  let chan _ = Array.make buflen 0. in
  (* TODO: multiple channels.. *)
  let buf = Array.init 2 chan in
  let decoder = ref None in
  let meta    = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let packet3 = ref None in
  let fill feed = 
    (* Decoder is created upon first decoding..*)
    let decoder,sample_freq = 
      match !decoder with
        | None -> 
           let packet1 =
             match !packet1 with
               | None ->
                  let p = Ogg.Stream.get_packet os in
                  packet1 := Some p; p
               | Some p -> p
           in
           let packet2 = 
             match !packet2 with
               | None -> 
                  let p = Ogg.Stream.get_packet os in
                  packet2 := Some p; p
               | Some p -> p
           in
           let packet3 = 
             match !packet3 with
               | None ->
                   let p = Ogg.Stream.get_packet os in
                   packet3 := Some p; p
               | Some p -> p
           in
           let d = Vorbis.Decoder.init packet1 packet2 packet3 in
           let info = Vorbis.Decoder.info d in
           meta := Some (Vorbis.Decoder.comments d);
           let samplerate = info.Vorbis.audio_samplerate in
           decoder := Some (d,samplerate);
           d,samplerate
        | Some d -> d
    in
    try
      let ret = Vorbis.Decoder.decode_pcm decoder os buf 0 buflen in
      let m = ! meta in
      meta := None;
      feed ((Array.map (fun x -> Array.sub x 0 ret) buf,sample_freq),m)
    with
      (* Apparently, we should hide this one.. *)
      | Vorbis.False -> raise Ogg.Not_enough_data
  in
  Ogg_demuxer.Audio fill

let () = Ogg_demuxer.ogg_decoders#register "vorbis" (check,decoder)

let create_gen enc freq m = 
  let p1,p2,p3 = Vorbis.Encoder.headerout_packetout enc m in
  let started  = ref false in
  let header_encoder os = 
    Ogg.Stream.put_packet os p1;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os = 
    Some (Vorbis.Skeleton.fisbone
           ~serialno:(Ogg.Stream.serialno os)
           ~samplerate:(Int64.of_int freq) ())
  in
  let stream_start os = 
    Ogg.Stream.put_packet os p2;
    Ogg.Stream.put_packet os p3;
    Ogg_encoder.flush_pages os
  in
  let data_encoder ogg_enc data os _ =
    if not !started then 
      started := true;
    let b,ofs,len = data.Ogg_encoder.data,data.Ogg_encoder.offset,
                    data.Ogg_encoder.length in
    Vorbis.Encoder.encode_buffer_float enc os b ofs len
  in
  let empty_data () = 
    Array.make 
       (Fmt.channels ())
       (Array.make 1 0.) 
  in
  let end_of_page p = 
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then
      Int64.minus_one
    else
      granulepos
  in
  let end_of_stream os = 
    (* Assert that at least some data was encoded.. *)
    if not !started then
      begin
        let b = empty_data () in
        Vorbis.Encoder.encode_buffer_float enc os b 0 (Array.length b.(0));
      end;
    Vorbis.Encoder.end_of_stream enc os
  in
  {
   Ogg_encoder.
    header_encoder = header_encoder;
    fisbone_packet = fisbone_packet;
    stream_start   = stream_start;
    data_encoder   = (Ogg_encoder.Audio_encoder data_encoder);
    rate           = Fmt.ticks_per_sample ();
    end_of_page    = end_of_page;
    end_of_stream  = end_of_stream
  }

let create_abr ~channels ~samplerate ~min_rate 
               ~max_rate ~average_rate ~metadata () = 
  let enc = 
    Vorbis.Encoder.create channels samplerate max_rate 
                          average_rate min_rate 
  in
  create_gen enc samplerate metadata

let create_cbr ~channels ~samplerate ~bitrate ~metadata () = 
  create_abr ~channels ~samplerate ~min_rate:bitrate 
             ~max_rate:bitrate ~average_rate:bitrate 
             ~metadata ()

let create ~channels ~samplerate ~quality ~metadata () = 
  let enc = 
    Vorbis.Encoder.create_vbr channels samplerate quality 
  in
  create_gen enc samplerate metadata

