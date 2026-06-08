(*
 * Copyright 2013 Savonet team
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

let src = ref ""
let dst = ref ""

let input_string chan len =
  let ans = Bytes.create len in
  (* TODO: check length *)
  ignore (input chan ans 0 len);
  Bytes.to_string ans

let input_int chan =
  let buf = input_string chan 4 in
  int_of_char buf.[0]
  + (int_of_char buf.[1] lsl 8)
  + (int_of_char buf.[2] lsl 16)
  + (int_of_char buf.[3] lsl 24)

let input_short chan =
  let buf = input_string chan 2 in
  int_of_char buf.[0] + (int_of_char buf.[1] lsl 8)

let vbr = ref 0
let bitrate = ref 64000

let aot_of_string = function
  | "MPEG4 AAC-LC" -> `Mpeg_4 `AAC_LC
  | "MPEG4 HE-AAC" -> `Mpeg_4 `HE_AAC
  | "MPEG4 HE-AAC v2" -> `Mpeg_4 `HE_AAC_v2
  | "MPEG4 AAC-LD" -> `Mpeg_4 `AAC_LD
  | "MPEG4 AAC-ELD" -> `Mpeg_4 `AAC_ELD
  | "MPEG2 AAC-LC" -> `Mpeg_2 `AAC_LC
  | "MPEG2 HE-AAC" -> `Mpeg_2 `HE_AAC
  | "MPEG2 HE-AAC v2" -> `Mpeg_2 `HE_AAC_v2
  | _ -> raise Fdkaac.Encoder.Unsupported_parameter

let string_of_aot = function
  | `Mpeg_4 `AAC_LC -> "MPEG4 AAC-LC"
  | `Mpeg_4 `HE_AAC -> "MPEG4 HE-AAC"
  | `Mpeg_4 `HE_AAC_v2 -> "MPEG4 HE-AAC v2"
  | `Mpeg_4 `AAC_LD -> "MPEG4 AAC-LD"
  | `Mpeg_4 `AAC_ELD -> "MPEG4 AAC-ELD"
  | `Mpeg_2 `AAC_LC -> "MPEG2 AAC-LC"
  | `Mpeg_2 `HE_AAC -> "MPEG2 HE-AAC"
  | `Mpeg_2 `HE_AAC_v2 -> "MPEG2 HE-AAC v2"

let aots =
  [
    "MPEG4 AAC-LC";
    "MPEG4 HE-AAC";
    "MPEG4 HE-AAC v2";
    "MPEG4 AAC-LD";
    "MPEG4 AAC-ELD";
    "MPEG2 AAC-LC";
    "MPEG2 HE-AAC";
    "MPEG42 HE-AAC v2";
  ]

let aot = ref (`Mpeg_4 `HE_AAC_v2)
let afterburner = ref true
let sbr = ref true
let usage = "usage: wav2aac [options] source destination"

let _ =
  let aots = List.map (Printf.sprintf "%S") aots in
  Arg.parse
    [
      ("--vbr", Arg.Int (fun i -> vbr := i), "Encode in variable bitrate.");
      ( "--bitrate",
        Arg.Int (fun b -> bitrate := b * 1000),
        "Bitrate, in kilobits per second, defaults to 64kbps" );
      ("--afterburner", Arg.Set afterburner, "Enable afterburner effect.");
      ( "--aot",
        Arg.String (fun x -> aot := aot_of_string x),
        Printf.sprintf "Audio object type. Possible values:\n    %s"
          (String.concat ",\n    " aots) );
      ("--no-sbr", Arg.Clear sbr, "Disable spectral band replication.");
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
  let enc = Fdkaac.Encoder.create channels in
  Fdkaac.Encoder.set enc (`Aot !aot);
  if !vbr <> 0 then Fdkaac.Encoder.set enc (`Bitrate_mode (`Variable !vbr))
  else begin
    Fdkaac.Encoder.set enc (`Bitrate_mode `Constant);
    Fdkaac.Encoder.set enc (`Bitrate !bitrate)
  end;
  Fdkaac.Encoder.set enc (`Transmux `Adts);
  Fdkaac.Encoder.set enc (`Samplerate infreq);
  Fdkaac.Encoder.set enc (`Afterburner !afterburner);
  Fdkaac.Encoder.set enc (`Sbr_mode !sbr);
  let buflen = 1024 in
  let data = Bytes.create buflen in
  let start = Unix.time () in
  Printf.printf "Input detected: PCM WAVE %d channels, %d Hz, %d bits\n%!"
    channels infreq bits;
  let br_info =
    if !vbr <> 0 then Printf.sprintf "VBR %d" !vbr
    else Printf.sprintf "%d kbps" (!bitrate / 1000)
  in
  Printf.printf
    "Encoding to: %s, %d channels, %d Hz, %s, afterburner: %b, sbr: %b\n\
     Please wait...\n\
     %!"
    (string_of_aot !aot) channels infreq br_info !afterburner !sbr;
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
  begin try
    while true do
      let len = input ic data 0 buflen in
      let data = Bytes.to_string data in
      if len = 0 then raise End_of_file;
      let ret = Fdkaac.Encoder.encode enc data 0 len in
      output_string oc ret
    done
  with
    | End_of_file -> ()
    | Fdkaac.Encoder.Error _ as e ->
        failwith
          (match Fdkaac.Encoder.string_of_exception e with
            | Some s -> s
            | None -> "Unknown error.")
  end;
  let ret =
    try Fdkaac.Encoder.flush enc
    with Fdkaac.Encoder.Unsupported_parameter ->
      Printf.printf "Could not flush.\n";
      ""
  in
  output_string oc ret;
  close_in ic;
  close_out oc;
  Printf.printf "Finished in %.0f seconds.\n" (Unix.time () -. start);
  Gc.full_major ()
