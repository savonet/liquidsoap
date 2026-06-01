(*
 * Copyright 2003 Savonet team
 *
 * This file is part of OCaml-Vorbis.
 *
 * OCaml-Vorbis is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-Vorbis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-Vorbis; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * An ogg to wav converter using OCaml-Vorbis stream interface.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

let bufsize = 1024
let src = ref ""
let dst = ref ""

open Unix

let output_int chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff));
  output_char chan (char_of_int ((n lsr 16) land 0xff));
  output_char chan (char_of_int ((n lsr 24) land 0xff))

let output_short chan n =
  output_char chan (char_of_int ((n lsr 0) land 0xff));
  output_char chan (char_of_int ((n lsr 8) land 0xff))

let progress_bar =
  let spin = ref 0 in
  fun title pos tot ->
    let nbeq = 40 in
    let n = min (100. *. float_of_int pos /. float_of_int tot) 100. in
    let e = int_of_float (n /. 100. *. float_of_int nbeq) in
    Printf.printf "\r%s %6.2f%% [" title n;
    for _ = 1 to e do
      Printf.printf "="
    done;
    if e != nbeq then Printf.printf ">";
    for _ = e + 2 to nbeq do
      Printf.printf " "
    done;
    Printf.printf "] ";
    incr spin;
    if !spin > 4 then spin := 1;
    Printf.printf "%c%!"
      (if n = 100. then ' '
       else (
         match !spin with
           | 1 -> '|'
           | 2 -> '/'
           | 3 -> '-'
           | 4 -> '\\'
           | _ -> failwith "this did not happen"))

let usage = "usage: stream2wav [options] source destination"

let _ =
  Arg.parse []
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
  let sync, _ = Ogg.Sync.create_from_file !src in
  let eos = ref false in
  let rec fill os =
    let page = Ogg.Sync.read sync in
    try
      Ogg.Stream.put_page os page;
      if Ogg.Page.eos page then eos := true
    with Ogg.Bad_data -> fill os
    (*Do not care about page that are not for us.. *)
  in
  (* Test whether the stream contains vorbis *)
  let test_vorbis () =
    (* Get First page *)
    let page = Ogg.Sync.read sync in
    (* Check whether this is a b_o_s *)
    if not (Ogg.Page.bos page) then raise Not_found;
    (* Create a stream with this ID *)
    let serial = Ogg.Page.serialno page in
    let os = Ogg.Stream.create ~serial () in
    Ogg.Stream.put_page os page;
    (* Get first packet *)
    let packet = Ogg.Stream.get_packet os in
    if not (Vorbis.Decoder.check_packet packet) then raise Not_found;
    fill os;
    let page = Ogg.Sync.read sync in
    Ogg.Stream.put_page os page;
    let packet2 = Ogg.Stream.get_packet os in
    fill os;
    let page = Ogg.Sync.read sync in
    Ogg.Stream.put_page os page;
    let packet3 = Ogg.Stream.get_packet os in
    let decoder = Vorbis.Decoder.init packet packet2 packet3 in
    Printf.printf "Got a vorbis stream !\n";
    flush_all ();
    (serial, os, decoder)
  in
  let rec init () =
    try test_vorbis ()
    with
    (* Not_found is not caught: ogg stream always start
        with all b_o_s and we don't care about sequenced streams here *)
    | Ogg.Bad_data | Not_found ->
      Printf.printf "This stream was not vorbis..\n";
      flush_all ();
      init ()
  in
  let _, os, decoder = init () in
  let infos = Vorbis.Decoder.info decoder in
  let vdr, cmt = Vorbis.Decoder.comments decoder in
  Printf.printf
    "Input stream characteristics: vorbis codec v%d, %d channels, %d Hz\n"
    infos.Vorbis.vorbis_version infos.Vorbis.audio_channels
    infos.Vorbis.audio_samplerate;
  Printf.printf "* vendor: %s\n" vdr;
  List.iter
    (fun (c, v) -> Printf.printf "* %s: %s\n" (String.lowercase_ascii c) v)
    cmt;
  flush_all ();
  (* Using vorbis to decode the ogg. *)
  print_newline ();
  Printf.printf "Decoding stream..\n";
  flush_all ();
  let tmpdst, oc =
    Filename.open_temp_file ~mode:[Open_binary] "stream2wav" ".raw"
  in
  (let chan _ = Array.make bufsize 0. in
   let buf = Array.init infos.Vorbis.audio_channels chan in
   try
     while true do
       try
         let r = Vorbis.Decoder.decode_pcm decoder os buf 0 bufsize in
         (* Yea, dirty quick hack ! *)
         let buf = Array.map (fun x -> Array.sub x 0 r) buf in
         let buf =
           Array.map (Array.map (fun x -> int_of_float (x *. 32767.))) buf
         in
         let chans = Array.length buf in
         Array.iteri
           (fun x _ ->
             for i = 0 to chans - 1 do
               output_short oc buf.(i).(x)
             done)
           buf.(0)
       with Ogg.Not_enough_data -> fill os
     done;
     close_out oc
   with _ -> close_out oc);
  Printf.printf "\n";

  (* Do the wav stuff. *)
  let samplerate = infos.Vorbis.audio_samplerate in
  let datalen = (stat tmpdst).st_size in
  let ic = open_in_bin tmpdst in
  let oc = open_out_bin !dst in
  output_string oc "RIFF";
  output_int oc (4 + 24 + 8 + datalen);
  output_string oc "WAVE";
  output_string oc "fmt ";
  output_int oc 16;
  output_short oc 1;
  (* WAVE_FORMAT_PCM *)
  output_short oc infos.Vorbis.audio_channels;
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
  (let buflen = 256 * 1024 in
   let buf = Bytes.create buflen in
   let r = ref 1 in
   let pos = ref 0 in
   let tot = datalen in
   while !r <> 0 do
     r := input ic buf 0 buflen;
     output oc buf 0 !r;
     pos := !pos + !r;
     progress_bar "Tagging wav: " !pos tot
   done);
  close_in ic;
  close_out oc;
  Unix.unlink tmpdst;
  Printf.printf "\n";
  Gc.full_major ()
