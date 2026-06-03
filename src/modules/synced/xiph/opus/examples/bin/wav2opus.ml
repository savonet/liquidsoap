(*
 * Copyright 2003 Savonet team
 *
 * This file is part of OCaml-opus.
 *
 * OCaml-opus is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * OCaml-opus is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with OCaml-opus; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * An wav to ogg converter using OCaml-opus.
  *
  * @author Samuel Mimram, and many others...
  *)

open Opus

let src = ref ""
let dst = ref ""
let buflen = ref 4096

let input_string chan len =
  let ans = Bytes.create len in
  (* TODO: check length *)
  ignore (input chan ans 0 len);
  Bytes.unsafe_to_string ans

let input_int chan =
  let buf = input_string chan 4 in
  int_of_char buf.[0]
  + (int_of_char buf.[1] lsl 8)
  + (int_of_char buf.[2] lsl 16)
  + (int_of_char buf.[3] lsl 24)

let input_short chan =
  let buf = input_string chan 2 in
  int_of_char buf.[0] + (int_of_char buf.[1] lsl 8)

let usage = "usage: wav2ogg [options] source destination"
let use_ba = ref false

let _ =
  Arg.parse
    [
      ( "--buflen",
        Arg.Int (fun i -> buflen := i),
        "Size of chunks successively encoded" );
      ("-ba", Arg.Set use_ba, "Use big arrays");
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
  let ic = open_in_bin !src in
  let oc = open_out_bin !dst in
  (* TODO: improve! *)
  if input_string ic 4 <> "RIFF" then invalid_arg "No RIFF tag";
  ignore (input_string ic 4);
  if input_string ic 4 <> "WAVE" then invalid_arg "No WAVE tag";
  if input_string ic 4 <> "fmt " then invalid_arg "No fmt tag";
  let _ = input_int ic in
  let _ = input_short ic in
  (* TODO: should be 1 *)
  let channels = input_short ic in
  let infreq = input_int ic in
  let _ = input_int ic in
  (* bytes / s *)
  let _ = input_short ic in
  (* block align *)
  let bits = input_short ic in
  let fos buf =
    let len = String.length buf / (2 * channels) in
    let ans = Array.init channels (fun _ -> Array.make len 0.) in
    for i = 0 to len - 1 do
      for c = 0 to channels - 1 do
        let n =
          int_of_char buf.[(2 * channels * i) + (2 * c)]
          + (int_of_char buf.[(2 * channels * i) + (2 * c) + 1] lsl 8)
        in
        let n =
          if n land (1 lsl 15) = 0 then n
          else (n land 0b111111111111111) - 32768
        in
        ans.(c).(i) <- float n /. 32768.;
        ans.(c).(i) <- max (-1.) (min 1. ans.(c).(i))
      done
    done;
    ans
  in
  let os = Ogg.Stream.create () in
  let enc =
    Encoder.create ~samplerate:infreq ~channels ~application:`Audio os
  in
  Ogg.Stream.put_packet os (Opus.Encoder.header enc);
  let ph, pb = Ogg.Stream.flush_page os in
  output_string oc (ph ^ pb);
  Ogg.Stream.put_packet os (Opus.Encoder.comments enc);
  let ph, pb = Ogg.Stream.flush_page os in
  output_string oc (ph ^ pb);
  let rem = ref (Array.make channels [||]) in
  let encode buf =
    let encoded, buf =
      if !use_ba then (
        let buf = fos buf in
        let buf = Array.mapi (fun c d -> Array.append d buf.(c)) !rem in
        let bbuf =
          Array.map
            (fun d ->
              Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout d)
            buf
        in
        let encoded =
          Encoder.encode_float_ba enc bbuf 0 (Bigarray.Array1.dim bbuf.(0))
        in
        (encoded, buf))
      else (
        let buf = fos buf in
        let buf = Array.mapi (fun c d -> Array.append d buf.(c)) !rem in
        let encoded = Encoder.encode_float enc buf 0 (Array.length buf.(0)) in
        (encoded, buf))
    in
    rem :=
      Array.map (fun d -> Array.sub d encoded (Array.length d - encoded)) buf
  in
  let start = Unix.time () in
  Printf.printf "Input detected: PCM WAVE %d channels, %d Hz, %d bits\n%!"
    channels infreq bits;
  Printf.printf "Encoding to: ogg/opus %d channels, %d Hz\nPlease wait...\n%!"
    channels infreq;
  (* skip headers *)
  let rec aux () =
    let tag = input_string ic 4 in
    match tag with
      | "LIST" ->
          let n = input_int ic in
          let _ = input_string ic n in
          aux ()
      | "data" -> ()
      | _ -> invalid_arg "No data tag"
  in
  aux ();
  let buflen = !buflen in
  let buf = Bytes.create buflen in
  begin try
    while true do
      try
        really_input ic buf 0 buflen;
        encode (Bytes.unsafe_to_string buf);
        while true do
          let ph, pb = Ogg.Stream.get_page os in
          output_string oc (ph ^ pb)
        done
      with Ogg.Not_enough_data -> ()
    done
  with End_of_file -> ()
  end;
  List.iter
    (fun (ph, pb) -> output_string oc (ph ^ pb))
    (Ogg.Stream.terminate os);
  close_in ic;
  close_out oc;
  Printf.printf "Finished in %.0f seconds.\n" (Unix.time () -. start);
  Gc.full_major ()
