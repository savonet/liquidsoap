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

exception Internal
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
  let rate = Lazy.force speex.Speex_format.samplerate in
  let header =
    Speex.Header.init ~frames_per_packet ~mode ~vbr ~nb_channels:channels ~rate
      ()
  in
  let enc = Speex.Encoder.init mode frames_per_packet in
  begin
    match speex.Speex_format.bitrate_control with
    | Speex_format.Vbr x ->
        Speex.Encoder.set enc Speex.SPEEX_SET_VBR 1;
        Speex.Encoder.set enc Speex.SPEEX_SET_VBR_QUALITY x
    | Speex_format.Abr x ->
        Speex.Encoder.set enc Speex.SPEEX_SET_ABR 1;
        Speex.Encoder.set enc Speex.SPEEX_SET_BITRATE x
    | Speex_format.Quality x -> Speex.Encoder.set enc Speex.SPEEX_SET_QUALITY x
  end;
  begin
    match speex.Speex_format.complexity with
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
  let remaining_init = if channels > 1 then [| [||]; [||] |] else [| [||] |] in
  let remaining = ref remaining_init in
  let data_encoder data os add_page =
    let b, ofs, len =
      (data.Ogg_muxer.data, data.Ogg_muxer.offset, data.Ogg_muxer.length)
    in
    let buf = Array.map (fun x -> Array.sub x ofs len) b in
    let buf =
      if channels > 1 then
        [|
          Array.append !remaining.(0) buf.(0);
          Array.append !remaining.(1) buf.(1);
        |]
      else [| Array.append !remaining.(0) buf.(0) |]
    in
    let len = Array.length buf.(0) in
    let status = ref 0 in
    let feed () =
      let n = !status in
      if (frame_size * n) + frame_size < len then (
        status := n + 1;

        (* Speex float API are values in - 32768. <= x <= 32767. ..
           I don't really trust this, it must be a bug,
           so using the int API. *)
        let f x =
          let x = int_of_float x in
          max (-32768) (min 32767 x)
        in
        let f x = Array.map (fun x -> f (32767. *. x)) x in
        if channels > 1 then
          [|
            f (Array.sub buf.(0) (frame_size * n) frame_size);
            f (Array.sub buf.(1) (frame_size * n) frame_size);
          |]
        else [| f (Array.sub buf.(0) (frame_size * n) frame_size) |] )
      else raise Internal
    in
    try
      while true do
        let page =
          if channels > 1 then Speex.Encoder.encode_page_int_stereo enc os feed
          else (
            let feed () =
              let x = feed () in
              x.(0)
            in
            Speex.Encoder.encode_page_int enc os feed )
        in
        add_page page
      done
    with Internal ->
      let n = !status in
      remaining :=
        if frame_size * n < len then
          if channels > 1 then
            [|
              Array.sub buf.(0) (frame_size * n) (len - (frame_size * n));
              Array.sub buf.(1) (frame_size * n) (len - (frame_size * n));
            |]
          else [| Array.sub buf.(0) (frame_size * n) (len - (frame_size * n)) |]
        else remaining_init
  in
  let end_of_page p =
    let granulepos = Ogg.Page.granulepos p in
    if granulepos < Int64.zero then Ogg_muxer.Unknown
    else Ogg_muxer.Time (Int64.to_float granulepos /. float rate)
  in
  let end_of_stream os = Speex.Encoder.eos enc os in
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
        let m = Utils.list_of_metadata (Meta_format.to_metadata m) in
        let title =
          try List.assoc "title" m
          with Not_found -> (
            try
              let s = List.assoc "uri" m in
              let title = Filename.basename s in
              try String.sub title 0 (String.rindex title '.')
              with Not_found -> title
            with Not_found -> "Unknown" )
        in
        let metadata = [("title", title)] @ List.remove_assoc "title" m in
        let enc = create speex ~metadata () in
        Ogg_muxer.register_track ?fill:speex.Speex_format.fill ogg_enc enc
      in
      let channels = if speex.Speex_format.stereo then 2 else 1 in
      let src_freq = float (Frame.audio_of_seconds 1.) in
      let dst_freq = float (Lazy.force speex.Speex_format.samplerate) in
      let encode = Ogg_encoder.encode_audio ~channels ~dst_freq ~src_freq () in
      { Ogg_encoder.reset; encode; id = None }
  | _ -> assert false

let () = Hashtbl.add Ogg_encoder.encoders "speex" create_speex
