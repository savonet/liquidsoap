(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

let check p =
  try
    let _ = Speex.Header.header_of_packet p in
    true
  with
    | _ -> false

let decoder os =
  let dec_p = ref None in
  let decoder = ref None in
  let com_p = ref None in
  let com = ref None in
  let fill feed =
    let dec,sample_freq,chans = 
      match !decoder with
        | None ->
           let dec_p = 
             match !dec_p with
               | None -> 
                   let packet = Ogg.Stream.get_packet os in
                   dec_p := Some packet;
                   packet
               | Some p -> p
           in
           let com_p = 
             match !com_p with
               | None ->
                   let packet = Ogg.Stream.get_packet os in
                   com_p := Some packet;
                   packet
               | Some p -> p
           in 
           let header = Speex.Header.header_of_packet dec_p in
           com := Some (Speex.Header.comments_of_packet com_p);
           let mode = header.Speex.Header.mode in
           let dec = Speex.Decoder.init mode in
           let sample_freq = header.Speex.Header.rate in
           let chans = header.Speex.Header.nb_channels in
           Speex.Decoder.set dec Speex.SPEEX_SET_SAMPLING_RATE sample_freq; 
           decoder := Some (dec,sample_freq,chans);
           dec,sample_freq,chans
        | Some (d,s,c) -> d,s,c
    in
    let len = ref 0 in
    let feed buf =
      let comments = !com in
      com := None; 
      let buf = Array.map (Array.map (fun x -> float x /. 32768.)) buf in
      len := !len + Array.length buf.(0);
      feed ((buf,sample_freq),comments)
    in
    try
      let decode dec os feed = 
        if chans = 2 then
          Speex.Decoder.decode_int_feed_stereo dec os feed
        else
          let feed x = 
            feed [|x|]
          in
          Speex.Decoder.decode_int_feed dec os feed
      in
      decode dec os feed
    with
      | Ogg.Not_enough_data ->
             if !len = 0 then
               raise Ogg.Not_enough_data
  in
  Ogg_demuxer.Audio fill

let () = Ogg_demuxer.ogg_decoders#register "speex" (check,decoder)

exception Internal
exception Invalid_settings of string

let create ~frames_per_packet ~mode ~vbr ~quality 
           ~stereo ~bitrate ~rate ~abr ~meta ~complexity () =
  if Fmt.channels () < 2 && stereo then
   raise (Invalid_settings "not enought channels");
  let nb_channels = 
    if stereo then 2 else 1
  in
  let header =
    Speex.Header.init ~frames_per_packet ~mode
                      ~vbr ~nb_channels
                      ~bitrate ~rate ()
  in
  let enc = Speex.Encoder.init mode frames_per_packet in
  if bitrate > 0 then
        Speex.Encoder.set enc Speex.SPEEX_SET_BITRATE bitrate;
  Speex.Encoder.set enc Speex.SPEEX_SET_COMPLEXITY complexity; 
  if abr > 0 then
    Speex.Encoder.set enc Speex.SPEEX_SET_ABR abr;
  Speex.Encoder.set enc Speex.SPEEX_SET_SAMPLING_RATE rate;
  let ivbr =
    if vbr then 1 else 0
  in
  Speex.Encoder.set enc Speex.SPEEX_SET_VBR ivbr;
  if quality > 0 then
    if vbr then
      Speex.Encoder.set enc Speex.SPEEX_SET_VBR_QUALITY quality
    else
      Speex.Encoder.set enc Speex.SPEEX_SET_QUALITY quality;
  let frame_size = Speex.Encoder.get enc Speex.SPEEX_GET_FRAME_SIZE in
  let p1,p2 = Speex.Header.encode_header_packetout header meta in
  let ret = ref (Some p2) in
  let header_encoder os = 
    Ogg.Stream.put_packet os p1;
    Ogg.Stream.flush_page os
  in
  let remaining_init =
    if stereo then
     [|[||];[||]|]
    else
     [|[||]|]
  in
  let remaining = ref remaining_init in
  let track_encoder ogg_enc data os =
    begin
     match !ret with
       | Some p -> 
          Ogg.Stream.put_packet os p;
          Ogg_encoder.add_page ogg_enc (Ogg.Stream.flush_page os);
          ret := None
       | None -> ()
    end;
    let b,ofs,len = data.Ogg_encoder.data,data.Ogg_encoder.offset,
                    data.Ogg_encoder.length in
    let buf = Array.map (fun x -> Array.sub x ofs len) b in
    let buf =
     if stereo then
         [|Array.append !remaining.(0) buf.(0);
           Array.append !remaining.(1) buf.(1)|]
     else
         [|Array.append !remaining.(0) buf.(0)|]
    in
    let len = Array.length buf.(0) in
    let status = ref 0 in
    let feed () =
      let n = !status in
      if frame_size*n + frame_size < len then
      ( status := n + 1;
        (* Speex float API are values in - 32768. <= x <= 32767. ..
           I don't really trust this, it must be a bug,
           so using the int API. *)
        let f x =
          let x = int_of_float x in
          max (-32768) (min 32767 x)
        in
        let f x = Array.map  (fun x -> f (32767.*.x)) x in
        if stereo then
          [| f (Array.sub buf.(0) (frame_size*n) frame_size);
             f (Array.sub buf.(1) (frame_size*n) frame_size) |]
        else
          [| f (Array.sub buf.(0) (frame_size*n) frame_size) |] )
      else
        raise Internal
    in
    try
      while true do
        let page =
          if stereo then
            Speex.Encoder.encode_page_int_stereo enc os feed
          else
            let feed () =
              let x = feed () in
              x.(0)
            in
            Speex.Encoder.encode_page_int enc os feed
        in
        Ogg_encoder.add_page ogg_enc page
      done
    with
      | Internal ->
          let n = !status in
          remaining := 
            if frame_size*n < len then
              if stereo then
                [|Array.sub buf.(0) (frame_size*n) (len - frame_size*n);
                  Array.sub buf.(1) (frame_size*n) (len - frame_size*n)|]
              else
                [|Array.sub buf.(0) (frame_size*n) (len - frame_size*n)|]
            else
              remaining_init
  in
  let end_of_stream os = 
    Speex.Encoder.eos enc os
  in
  header_encoder,(Ogg_encoder.Audio_encoder track_encoder),
  end_of_stream

