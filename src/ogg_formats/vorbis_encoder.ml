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

let create_gen enc freq m =
  let encoder = Configure.vendor in
  let p1, p2, p3 = Vorbis.Encoder.headerout_packetout ~encoder enc m in
  let started = ref false in
  let header_encoder os =
    Ogg.Stream.put_packet os p1 ;
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os =
    Some
      (Vorbis.Skeleton.fisbone ~serialno:(Ogg.Stream.serialno os)
         ~samplerate:(Int64.of_int freq) ())
  in
  let stream_start os =
    Ogg.Stream.put_packet os p2 ;
    Ogg.Stream.put_packet os p3 ;
    Ogg_muxer.flush_pages os
  in
  let data_encoder data os _ =
    if not !started then started := true ;
    let b, ofs, len =
      (data.Ogg_muxer.data, data.Ogg_muxer.offset, data.Ogg_muxer.length)
    in
    Vorbis.Encoder.encode_buffer_float enc os b ofs len
  in
  let empty_data () =
    Array.make (Lazy.force Frame.audio_channels) (Array.make 1 0.)
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else Ogg_muxer.Time (Int64.to_float granulepos /. float freq)
  in
  let end_of_stream os =
    (* Assert that at least some data was encoded.. *)
    if not !started then (
      let b = empty_data () in
      Vorbis.Encoder.encode_buffer_float enc os b 0 (Array.length b.(0)) ) ;
    Vorbis.Encoder.end_of_stream enc os
  in
  {
    Ogg_muxer.header_encoder;
    fisbone_packet;
    stream_start;
    data_encoder= Ogg_muxer.Audio_encoder data_encoder;
    end_of_page;
    end_of_stream;
  }

(** Rates are given in bits per seconds,
  * i.e. 128000 instead of 128.. *)
let create_abr ~channels ~samplerate ~min_rate ~max_rate ~average_rate
    ~metadata () =
  let min_rate, max_rate, average_rate =
    (1000 * min_rate, 1000 * max_rate, 1000 * average_rate)
  in
  let enc =
    Vorbis.Encoder.create channels samplerate max_rate average_rate min_rate
  in
  create_gen enc samplerate metadata

let create_cbr ~channels ~samplerate ~bitrate ~metadata () =
  create_abr ~channels ~samplerate ~min_rate:bitrate ~max_rate:bitrate
    ~average_rate:bitrate ~metadata ()

let create ~channels ~samplerate ~quality ~metadata () =
  let enc = Vorbis.Encoder.create_vbr channels samplerate quality in
  create_gen enc samplerate metadata

let create_vorbis = function
  | Ogg_format.Vorbis vorbis ->
      let channels = vorbis.Vorbis_format.channels in
      let samplerate = Lazy.force vorbis.Vorbis_format.samplerate in
      let reset ogg_enc m =
        let m = Meta_format.to_metadata m in
        let metadata = Vorbis.tags m () in
        let enc =
          (* For ABR, a value of -1 means unset.. *)
          let f x = match x with Some x -> x | None -> -1 in
          match vorbis.Vorbis_format.mode with
            | Vorbis_format.ABR (min_rate, average_rate, max_rate) ->
                let min_rate, average_rate, max_rate =
                  (f min_rate, f average_rate, f max_rate)
                in
                create_abr ~channels ~samplerate ~max_rate ~average_rate
                  ~min_rate ~metadata ()
            | Vorbis_format.CBR bitrate ->
                create_cbr ~channels ~samplerate ~bitrate ~metadata ()
            | Vorbis_format.VBR quality ->
                create ~channels ~samplerate ~quality ~metadata ()
        in
        Ogg_muxer.register_track ?fill:vorbis.Vorbis_format.fill ogg_enc enc
      in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float samplerate in
      let encode = Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq () in
      {Ogg_encoder.reset; encode; id= None}
  | _ ->
      assert false

let () = Hashtbl.add Ogg_encoder.encoders "vorbis" create_vorbis
