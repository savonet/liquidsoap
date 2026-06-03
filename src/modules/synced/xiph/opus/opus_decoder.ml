(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of ocaml-opus.
 *
 * ocaml-opus is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-opus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-opus; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *)

let check = Opus.Decoder.check_packet
let buflen = Opus.recommended_frame_size
let decoder_samplerate = ref 48000

let decoder ~fill:_ os =
  let decoder = ref None in
  let packet1 = ref None in
  let packet2 = ref None in
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
          let dec =
            Opus.Decoder.create ~samplerate:!decoder_samplerate packet1 packet2
          in
          let chans = Opus.Decoder.channels dec in
          let meta = Opus.Decoder.comments dec in
          decoder := Some (dec, chans, meta);
          (dec, chans, meta)
      | Some dec -> dec
  in
  let info () =
    let _, chans, meta = init () in
    ({ Ogg_decoder.channels = chans; sample_rate = !decoder_samplerate }, meta)
  in
  let restart ~fill:_ new_os =
    os := new_os;
    decoder := None;
    ignore (init ())
  in
  let decode ~decode_float ~make_float ~sub_float feed =
    let dec, chans, _ = init () in
    let chan _ = make_float buflen in
    let buf = Array.init chans chan in
    let ret = decode_float dec !os buf 0 buflen in
    feed (Array.map (fun x -> sub_float x 0 ret) buf)
  in
  let decoder ~decode_float ~make_float ~sub_float =
    {
      Ogg_decoder.name = "opus";
      info;
      decode = decode ~decode_float ~make_float ~sub_float;
      restart;
      samples_of_granulepos = (fun x -> x);
    }
  in
  Ogg_decoder.Audio_both
    ( decoder ~decode_float:Opus.Decoder.decode_float
        ~make_float:(fun len -> Array.make len 0.)
        ~sub_float:Array.sub,
      decoder ~decode_float:Opus.Decoder.decode_float_ba
        ~make_float:(Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout)
        ~sub_float:Bigarray.Array1.sub )

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "opus" (check, decoder)
