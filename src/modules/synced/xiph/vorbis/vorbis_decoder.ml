(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-vorbis.
 *
 * Ocaml-vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *)

let check = Vorbis.Decoder.check_packet
let buflen = 1024

let decoder ~fill:_ os =
  let decoder = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
  let packet3 = ref None in
  let os = ref os in
  let init () =
    match !decoder with
      | None ->
          let packet1 =
            match !packet1 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet1 := Some p;
                  p
              | Some p -> p
          in
          let packet2 =
            match !packet2 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet2 := Some p;
                  p
              | Some p -> p
          in
          let packet3 =
            match !packet3 with
              | None ->
                  let p = Ogg.Stream.get_packet !os in
                  packet3 := Some p;
                  p
              | Some p -> p
          in
          let d = Vorbis.Decoder.init packet1 packet2 packet3 in
          let info = Vorbis.Decoder.info d in
          let meta = Vorbis.Decoder.comments d in
          decoder := Some (d, info, meta);
          (d, info, meta)
      | Some d -> d
  in
  let info () =
    let _, info, meta = init () in
    ( {
        Ogg_decoder.channels = info.Vorbis.audio_channels;
        sample_rate = info.Vorbis.audio_samplerate;
      },
      meta )
  in
  let restart ~fill:_ new_os =
    os := new_os;
    let d, _, _ = init () in
    Vorbis.Decoder.restart d
  in
  let decode ~decode_pcm ~make_pcm ~sub_pcm feed =
    let decoder, info, _ = init () in
    let chan _ = make_pcm buflen in
    let buf = Array.init info.Vorbis.audio_channels chan in
    try
      let ret = decode_pcm decoder !os buf 0 buflen in
      feed (Array.map (fun x -> sub_pcm x 0 ret) buf)
    with
    (* Apparently, we should hide this one.. *)
    | Vorbis.False ->
      raise Ogg.Not_enough_data
  in
  let decoder ~decode_pcm ~make_pcm ~sub_pcm =
    {
      Ogg_decoder.name = "vorbis";
      info;
      decode = decode ~decode_pcm ~make_pcm ~sub_pcm;
      restart;
      samples_of_granulepos = (fun x -> x);
    }
  in
  Ogg_decoder.Audio_both
    ( decoder ~decode_pcm:Vorbis.Decoder.decode_pcm
        ~make_pcm:(fun len -> Array.create_float len)
        ~sub_pcm:Array.sub,
      decoder ~decode_pcm:Vorbis.Decoder.decode_pcm_ba
        ~make_pcm:(Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout)
        ~sub_pcm:Bigarray.Array1.sub )

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "vorbis" (check, decoder)
