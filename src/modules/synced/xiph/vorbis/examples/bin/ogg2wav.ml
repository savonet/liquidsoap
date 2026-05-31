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
  * An ogg to wav converter using OCaml-Vorbis.
  *
  * @author Samuel Mimram
  *)

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

let usage = "usage: ogg2wav [options] source destination"
let use_ba = ref false
let use_alloc = ref false

let _ =
  Arg.parse
    [
      ("-ba", Arg.Set use_ba, "Use big arrays");
      ("-alloc", Arg.Set use_alloc, "Use alloc API");
    ]
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

  let df, dfd = Vorbis.File.Decoder.openfile !src in
  let infos = Vorbis.File.Decoder.info df (-1) in
  let vdr, cmt = Vorbis.File.Decoder.comments df (-1) in
  let duration = Vorbis.File.Decoder.duration df (-1) in
  let samples = Vorbis.File.Decoder.samples df (-1) in
  let chans = infos.Vorbis.audio_channels in
  Printf.printf
    "Input file characteristics: vorbis codec v%d, %d channels, %d Hz, %.02f \
     s, %d samples\n"
    infos.Vorbis.vorbis_version chans infos.Vorbis.audio_samplerate duration
    samples;
  Printf.printf "* vendor: %s\n" vdr;
  List.iter
    (fun (c, v) -> Printf.printf "* %s: %s\n" (String.lowercase_ascii c) v)
    cmt;

  (* Using vorbis to decode the ogg. *)
  Printf.printf "\n";
  let tmpdst, oc =
    Filename.open_temp_file ~mode:[Open_binary] "ogg2wav" ".raw"
  in
  (let decode =
     if !use_ba then (
       let process len buf =
         for i = 0 to len - 1 do
           for c = 0 to chans - 1 do
             let s = int_of_float (buf.(c).{i} *. 32767.) in
             output_short oc s
           done
         done;
         len
       in
       if !use_alloc then fun () ->
         if chans = 0 then 0
         else (
           let buf = Vorbis.File.Decoder.decode_float_alloc_ba df bufsize in
           process (Bigarray.Array1.dim buf.(0)) buf)
       else (
         let buf =
           Array.init chans (fun _ ->
               Bigarray.Array1.create Bigarray.float32 Bigarray.c_layout bufsize)
         in
         fun () ->
           let len = Vorbis.File.Decoder.decode_float_ba df buf 0 bufsize in
           process len buf))
     else (
       let buf = Bytes.create (16 * bufsize) in
       fun () ->
         let r = Vorbis.File.Decoder.decode df buf 0 bufsize in
         output oc buf 0 r;
         r / 4)
   in
   let pos = ref 0 in
   let tot = samples in
   try
     while true do
       let r = decode () in
       pos := !pos + r;
       progress_bar "Decoding ogg:" !pos tot
     done;
     close_out oc;
     Unix.close dfd
   with End_of_file ->
     close_out oc;
     Unix.close dfd);
  Printf.printf "\n";

  (* Do the wav stuff. *)
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
  output_short oc 2;
  (* channels *)
  output_int oc 44100;
  (* freq *)
  output_int oc (44100 * 2 * 2);
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
