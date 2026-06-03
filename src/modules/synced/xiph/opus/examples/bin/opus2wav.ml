(*
 * Copyright 2003 Savonet team
 *
 * This file is part of OCaml-Opus.
 *
 * OCaml-Opus is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Opus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-Opus; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * An opus to wav converter using OCaml-Opus.
  *
  * @author Samuel Mimram
  *)

let src = ref ""
let dst = ref ""

let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))

let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))

let usage = "usage: opus2wav [options] source destination"
let use_ba = ref false

let () =
  Arg.parse
    [("-ba", Arg.Set use_ba, "Use big arrays")]
    (let pnum = ref (-1) in
     fun s ->
       incr pnum;
       match !pnum with
         | 0 -> src := s
         | 1 -> dst := s
         | _ ->
             Printf.eprintf "Error: too many arguments\n";
             exit 1)
    usage;
  if !src = "" || !dst = "" then (
    Printf.printf "%s\n" usage;
    exit 1);

  let sync, fd = Ogg.Sync.create_from_file !src in
  Printf.printf "Checking file.\n%!";
  let os, p1 =
    let page = Ogg.Sync.read sync in
    assert (Ogg.Page.bos page);
    let serial = Ogg.Page.serialno page in
    Printf.printf "Testing stream %nx.%!\n" serial;
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page;
    let packet = Ogg.Stream.get_packet os in
    assert (Opus.Decoder.check_packet packet);
    Printf.printf "Found an opus stream!\n%!";
    (os, packet)
  in
  let page = Ogg.Sync.read sync in
  Ogg.Stream.put_page os page;
  let p2 = Ogg.Stream.get_packet os in
  let samplerate = 48000 in
  Printf.printf "Creating decoder...\n%!";
  let dec = Opus.Decoder.create ~samplerate p1 p2 in
  let chans = Opus.Decoder.channels dec in
  Printf.printf "Channels: %d\n%!" chans;
  let vendor, comments = Opus.Decoder.comments dec in
  Printf.printf "Vendor: %s\nComments:\n%!" vendor;
  List.iter (fun (l, v) -> Printf.printf "- %s = %s\n%!" l v) comments;
  Printf.printf "done.\n%!";

  Printf.printf "Decoding...%!";
  let max_frame_size = 960 * 6 in
  let buflen = max_frame_size in
  let outbuf = Array.make chans ([||] : float array) in
  let decode () =
    if !use_ba then (
      let buf =
        Array.init chans (fun _ ->
            Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout buflen)
      in
      let len = Opus.Decoder.decode_float_ba dec os buf 0 buflen in
      for c = 0 to chans - 1 do
        let pcm = Array.make len 0. in
        for i = 0 to len - 1 do
          pcm.(i) <- buf.(c).{i}
        done;
        outbuf.(c) <- pcm
      done)
    else (
      let buf = Array.init chans (fun _ -> Array.make buflen 0.) in
      let len = Opus.Decoder.decode_float dec os buf 0 buflen in
      for c = 0 to chans - 1 do
        outbuf.(c) <- Array.append outbuf.(c) (Array.sub buf.(c) 0 len)
      done)
  in
  (try
     while true do
       try decode ()
       with Ogg.Not_enough_data ->
         let page = Ogg.Sync.read sync in
         if Ogg.Page.serialno page = Ogg.Stream.serialno os then
           Ogg.Stream.put_page os page
     done
   with Ogg.End_of_stream -> ());
  Printf.printf "done.\n%!";
  Unix.close fd;

  let len = Array.length outbuf.(0) in
  let datalen = 2 * len in
  let oc = open_out_bin !dst in
  output_string oc "RIFF";
  output_int oc (4 + 24 + 8 + datalen);
  output_string oc "WAVE";
  output_string oc "fmt ";
  output_int oc 16;
  output_short oc 1;
  (* WAVE_FORMAT_PCM *)
  output_short oc 2;
  (* channels *)
  output_int oc samplerate;
  (* freq *)
  output_int oc (samplerate * 2 * 2);
  (* bytes / s *)
  output_short oc (2 * 2);
  (* block alignment *)
  output_short oc 16;
  (* bits per sample *)
  output_string oc "data";
  output_int oc datalen;

  for i = 0 to len - 1 do
    for c = 0 to chans - 1 do
      let x = outbuf.(c).(i) in
      let x = int_of_float (x *. 32767.) in
      output_short oc x
    done
  done;
  close_out oc;
  Gc.full_major ()
