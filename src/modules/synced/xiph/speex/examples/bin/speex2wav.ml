(*
 * Copyright 2008 Savonet team
 *
 * This file is part of OCaml-speex.
 *
 * OCaml-speex is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-speex is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-speex; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * A speex to wav converter using OCaml-speex.
  *
  * @author Samuel Mimram
  * @author Romain Beauxis
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

let usage = "usage: speex2wav source destination"
let () = Speex_decoder.register ()

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
  let dec, fd = Ogg_decoder.init_from_file !src in
  let { Ogg_decoder.audio_track; _ } = Ogg_decoder.get_standard_tracks dec in
  let audio_track =
    match audio_track with
      | None ->
          Printf.eprintf "Error: no audio track\n";
          exit 1
      | Some audio_track -> audio_track
  in
  let { Ogg_decoder.channels; sample_rate }, (encoder, comments) =
    Ogg_decoder.audio_info dec audio_track
  in
  Printf.printf "Encoder: %s\n" encoder;
  let print_comment (k, v) = Printf.printf "%s: %s\n" k v in
  Printf.printf "Comments:\n";
  List.iter print_comment comments;
  Printf.printf "\n";
  Printf.printf "Input file characteristics: %d channels, %d Hz\n" channels
    sample_rate;
  (* Using speex to decode the ogg. *)
  Printf.printf "\nDecoding...\n";
  flush_all ();
  let tmpdst, oc =
    Filename.open_temp_file ~mode:[Open_binary] "speex2wav" ".raw"
  in
  (try
     while true do
       Ogg_decoder.decode_audio dec audio_track (fun data ->
           let s1 = data.(0) in
           Array.iteri
             (fun n _ ->
               Array.iter
                 (fun s ->
                   let sample = int_of_float (s.(n) *. 32767.) in
                   output_short oc sample)
                 data)
             s1)
     done
   with Ogg_decoder.End_of_stream -> close_out oc);
  Printf.printf "Decoding finished, writing WAV..\n";
  Unix.close fd;
  (* Do the wav stuff. *)
  let datalen = (Unix.stat tmpdst).st_size in
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
  output_int oc sample_rate;
  (* freq *)
  output_int oc (sample_rate * 2 * 2);
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
   while !r <> 0 do
     r := input ic buf 0 buflen;
     output oc buf 0 !r;
     pos := !pos + !r
   done);
  close_in ic;
  close_out oc;
  Unix.unlink tmpdst;
  Printf.printf "Done !\n";
  Gc.full_major ()
