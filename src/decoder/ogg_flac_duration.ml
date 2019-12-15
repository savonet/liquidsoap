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

(** Read duration of ogg/flac files. *)

let duration file =
  let sync, fd = Ogg.Sync.create_from_file file in
  Tutils.finalize
    ~k:(fun () -> Unix.close fd)
    (fun _ ->
      let test_flac () =
        (* Get First page *)
        let page = Ogg.Sync.read sync in
        (* Check wether this is a b_o_s *)
        if not (Ogg.Page.bos page) then raise Flac.Decoder.Not_flac;
        (* Create a stream with this ID *)
        let serial = Ogg.Page.serialno page in
        let os = Ogg.Stream.create ~serial () in
        Ogg.Stream.put_page os page;
        let packet = Ogg.Stream.get_packet os in
        (* Test header. Do not catch anything, first page should be sufficient *)
        if not (Ogg_flac.Decoder.check_packet packet) then raise Not_found;
        let fill () =
          let page = Ogg.Sync.read sync in
          if Ogg.Page.serialno page = serial then Ogg.Stream.put_page os page
        in
        let callbacks = Ogg_flac.Decoder.get_callbacks (fun _ -> ()) in
        let dec = Ogg_flac.Decoder.create packet os in
        let rec info () =
          try Flac.Decoder.init dec callbacks
          with Ogg.Not_enough_data ->
            fill ();
            info ()
        in
        info ()
      in
      (* Now find a flac stream *)
      let rec init () = try test_flac () with Not_found -> init () in
      let _, info, _ = init () in
      let samples = Int64.to_float info.Flac.Decoder.total_samples in
      (* If we have no sample, we play it safe and raise
       * Not_found *)
      if samples <= 0. then raise Not_found;
      samples /. float info.Flac.Decoder.sample_rate)

let () = Request.dresolvers#register "OGG/FLAC" duration
