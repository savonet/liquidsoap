(*
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of OCaml-mad.
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * An mp3 to wav converter using OCaml-Mad.
  *
  * @author Samuel Mimram
  *)

(* $Id$ *)

(* TODO: we don't need the tagging phase *)

open Unix

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

let usage = "usage: mp32wav [options] source destination"

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

  (* Using mad to decode the mp3. *)
  let tmpdst, oc =
    Filename.open_temp_file ~mode:[Open_binary] "mp32wav" ".raw"
  in
  let is_first = ref true in
  let channels = ref 2 in
  let samplerate = ref 44100 in
  let tot = (Unix.stat !src).Unix.st_size in
  let fd = Unix.openfile !src [Unix.O_RDONLY] 0o600 in
  let read = Unix.read fd in
  let seek pos = Unix.lseek fd pos Unix.SEEK_SET in
  let tell () = Unix.lseek fd 0 Unix.SEEK_CUR in
  Mad.skip_id3tags ~read ~seek ~tell;
  let mf = Mad.openstream (Unix.read fd) in

  (try
     while true do
       let d = Mad.decode_frame mf in
       if !is_first then begin
         let sr, ch, _ = Mad.get_output_format mf in
         samplerate := sr;
         channels := ch;
         is_first := false
       end;
       output_string oc d;
       progress_bar "Decoding mp3:" (tell ()) tot
     done
   with Mad.End_of_stream ->
     close_out oc;
     Mad.close mf);
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
  output_int oc !samplerate;
  (* freq *)
  output_int oc (!samplerate * !channels * 2);
  (* bytes / s *)
  output_short oc (!channels * 2);
  (* block alignment *)
  output_short oc 16;
  (* bits per sample *)
  output_string oc "data";
  (* TODO: uncomment the following line if you only hear noise *)
  (* output_char oc '\000'; *)
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
  Printf.printf "\n"
