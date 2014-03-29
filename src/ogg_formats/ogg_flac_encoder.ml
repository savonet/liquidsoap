(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2014 Savonet team

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

let create_encoder ~flac ~comments () =
  let samplerate = flac.Encoder.Flac.samplerate in
  let p = 
    { Flac.Encoder.
       channels = flac.Encoder.Flac.channels ;
       bits_per_sample = flac.Encoder.Flac.bits_per_sample ;
       sample_rate = samplerate ;
       compression_level = Some (flac.Encoder.Flac.compression);
       total_samples = None;
    }
  in
  let enc = ref None in
  let started = ref false in
  let get_enc os = 
    match !enc with
      | Some x -> x
      | None ->
         let x = 
            Ogg_flac.Encoder.create ~comments p os
         in
         enc := Some x ;
         x
  in
  let cb = Ogg_flac.Encoder.callbacks in
  let empty_data () =
    Array.make
       (Lazy.force Frame.audio_channels)
       (Array.make 1 0.)
  in
  let header_encoder os =
    let (_,p,_) = get_enc os in
    Ogg.Stream.put_packet os p; 
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os =
    Some (Ogg_flac.Skeleton.fisbone
           ~serialno:(Ogg.Stream.serialno os)
           ~samplerate:(Int64.of_int samplerate) ())
  in
  let stream_start os = 
    let (_,_,l) = get_enc os in
    List.iter (Ogg.Stream.put_packet os) l;
    Ogg_muxer.flush_pages os
  in
  let data_encoder data os _ =
    if not !started then
      started := true;
    let b,ofs,len = data.Ogg_muxer.data,data.Ogg_muxer.offset,
                    data.Ogg_muxer.length in
    let b = Array.map (fun x -> Array.sub x ofs len) b in
    let (enc,_,_) = get_enc os in
    Flac.Encoder.process enc cb b
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then
      Ogg_muxer.Unknown
    else
      Ogg_muxer.Time (Int64.to_float granulepos /. (float samplerate))
  in
  let end_of_stream os =
    let (enc,_,_) = get_enc os in
    (* Assert that at least some data was encoded.. *)
    if not !started then
      begin
        let b = empty_data () in
        Flac.Encoder.process enc cb b;
      end;
    Flac.Encoder.finish enc cb ;
    Ogg_flac.Encoder.finish enc
  in
  {
   Ogg_muxer.
    header_encoder = header_encoder;
    fisbone_packet = fisbone_packet;
    stream_start   = stream_start;
    data_encoder   = (Ogg_muxer.Audio_encoder data_encoder);
    end_of_page    = end_of_page;
    end_of_stream  = end_of_stream
  }

let create_flac = 
  function 
    | Encoder.Ogg.Flac flac -> 
       let reset ogg_enc m =
         let comments = 
           Utils.list_of_metadata (Encoder.Meta.to_metadata m) 
         in 
         let enc =
           create_encoder ~flac ~comments ()
         in
         Ogg_muxer.register_track ?fill:flac.Encoder.Flac.fill ogg_enc enc
       in
       let src_freq = float (Frame.audio_of_seconds 1.) in
       let dst_freq = float flac.Encoder.Flac.samplerate in
       let channels = flac.Encoder.Flac.channels in
       let encode =
         Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq ()
       in
       { 
        Ogg_encoder.
           encode = encode ;
           reset  = reset  ;
           id     = None
       }
    | _ -> assert false

let () = Hashtbl.add Ogg_encoder.encoders "flac" create_flac
