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

let create_encoder ~opus ~comments () =
  let samplerate = opus.Encoder.Opus.samplerate in
  let channels = opus.Encoder.Opus.channels in
  let frame_size = opus.Encoder.Opus.frame_size in
  let application = match opus.Encoder.Opus.application with
    | None -> `Audio
    | Some a -> a
  in
  let pending = ref [||] in
  let enc = ref None in
  let get_enc os = 
    match !enc with
      | Some x -> x
      | None ->
         let x =
           Opus.Encoder.create ~comments ~channels ~samplerate ~application os
         in
         Opus.Encoder.apply_control (`Set_bitrate opus.Encoder.Opus.bitrate) x;
         begin match opus.Encoder.Opus.mode with
           | Encoder.Opus.CBR ->
               Opus.Encoder.apply_control (`Set_vbr false) x
           | Encoder.Opus.VBR b ->
               Opus.Encoder.apply_control (`Set_vbr true) x;
               Opus.Encoder.apply_control (`Set_vbr_constraint b) x
         end;
         let maybe name = fun value ->
           ignore
             (Utils.maybe 
               (fun value -> Opus.Encoder.apply_control (name value) x) 
               value)
         in 
         maybe (fun (v) -> `Set_complexity v) opus.Encoder.Opus.complexity;
         maybe
           (fun (v) -> `Set_max_bandwidth v)
           opus.Encoder.Opus.max_bandwidth ;
         maybe (fun (v) -> `Set_signal v) opus.Encoder.Opus.signal;
         Opus.Encoder.apply_control (`Set_dtx opus.Encoder.Opus.dtx) x;
         enc := Some x ;
         x
  in
  let header_encoder os =
    let enc = get_enc os in
    Ogg.Stream.put_packet os (Opus.Encoder.header enc); 
    Ogg.Stream.flush_page os
  in
  let fisbone_packet os =
    None
  in
  let stream_start os = 
    let enc = get_enc os in
    Ogg.Stream.put_packet os (Opus.Encoder.comments enc);
    Ogg_muxer.flush_pages os
  in
  let data_encoder data os _ =
    let enc = get_enc os in
    let data =
      Array.map (fun buf ->
        Array.sub buf data.Ogg_muxer.offset data.Ogg_muxer.length) 
        data.Ogg_muxer.data
    in
    let data = 
      if Array.length !pending == 0 then
        data
      else
        Array.mapi (fun i channel -> Array.append !pending.(i) channel) data
    in
    let ret = 
      try
        Opus.Encoder.encode_float
          ~frame_size enc data 0 (Array.length data.(0))
      with
        | Opus.Buffer_too_small -> Array.length data.(0)
    in
    if ret > 0 then
      pending := Array.map 
        (fun channel -> Array.sub channel ret ((Array.length channel) - ret))
        data
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then
      Ogg_muxer.Unknown
    else
      Ogg_muxer.Time (Int64.to_float granulepos /. 48000.)
  in
  let end_of_stream os =
    let enc = get_enc os in
    Opus.Encoder.eos enc
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

let create_opus = 
  function 
    | Encoder.Ogg.Opus opus -> 
       let reset ogg_enc m =
         let comments = 
           Utils.list_of_metadata (Encoder.Meta.to_metadata m) 
         in 
         let enc =
           create_encoder ~opus ~comments ()
         in
         Ogg_muxer.register_track ?fill:opus.Encoder.Opus.fill ogg_enc enc
       in
       let src_freq = float (Frame.audio_of_seconds 1.) in
       let dst_freq = float opus.Encoder.Opus.samplerate in
       let channels = opus.Encoder.Opus.channels in
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

let () = Hashtbl.add Ogg_encoder.encoders "opus" create_opus
