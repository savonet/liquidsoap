(*
 * Copyright 2003-2011 Savonet team
 *
 * This file is part of Ocaml-flac.
 *
 * Ocaml-flac is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-flac is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-flac; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let check = Flac_ogg.Decoder.check_packet

let mk_decoder ~fill ~write os =
  let dec, info, m = Flac_ogg.Decoder.create ~fill ~write os in
  let meta = match m with None -> ("Unknown vendor", []) | Some x -> x in
  (dec, info, meta)

let decoder ~fill os =
  let decoder = ref None in
  let write_ref = ref (fun _ -> ()) in
  let write ret =
    let fn = !write_ref in
    fn ret
  in
  let get_decoder () =
    match !decoder with
      | None ->
          let dec, info, meta = mk_decoder ~fill ~write os in
          decoder := Some (dec, info, meta);
          (dec, info, meta)
      | Some d -> d
  in
  let info () =
    let _, info, m = get_decoder () in
    ( {
        Ogg_decoder.channels = info.Flac.Decoder.channels;
        sample_rate = info.Flac.Decoder.sample_rate;
      },
      m )
  in
  let decode write =
    write_ref := write;
    let decoder, _, _ = get_decoder () in
    match Flac.Decoder.state decoder with
      | `Search_for_metadata | `Read_metadata | `Search_for_frame_sync
      | `Read_frame ->
          Flac.Decoder.process decoder
      (* Ogg decoder is responsible for detecting end of stream vs. end of track. *)
      | _ -> raise Ogg.Not_enough_data
  in
  let restart ~fill new_os =
    (write_ref := fun _ -> ());
    let d, _, _ = get_decoder () in
    (* Flush error are very unlikely. *)
    assert (Flac.Decoder.flush d);
    decoder := Some (mk_decoder ~fill ~write new_os)
  in
  Ogg_decoder.Audio
    {
      Ogg_decoder.name = "flac";
      info;
      decode;
      restart;
      samples_of_granulepos = (fun x -> x);
    }

let register () = Hashtbl.add Ogg_decoder.ogg_decoders "flac" (check, decoder)
