(*
 * Copyright 2008 Savonet team
 *
 * This file is part of ocaml-speex.
 *
 * ocaml-speex is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-speex is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with ocaml-speex; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * An wav to ogg converter using ocaml-speex.
  *
  * @author Samuel Mimram, Romain Beauxis, and many others...
  *)

open Speex

let src = ref ""
let dst = ref ""

let input_string chan len =
  let ans = Bytes.create len in
  really_input chan ans 0 len;
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

let usage = "usage: wav2speex [options] source destination"
let float = ref false
let mode = ref Wideband
let fpp = ref 5
let vbr = ref false
let quality = ref 7

let _ =
  let f = Printf.sprintf in
  let string_of_mode m =
    match m with
      | Narrowband -> "narrowband"
      | Wideband -> "wideband"
      | Ultra_wideband -> "ultra-wideband"
  in
  Arg.parse
    [
      ( "--float",
        Arg.Unit (fun _ -> float := true),
        f "Use floats for decoding. Default: %b" !float );
      ( "--mode",
        Arg.String
          (fun b ->
            match b with
              | "narrowband" -> mode := Narrowband
              | "wideband" -> mode := Wideband
              | "ultra-wideband" -> mode := Ultra_wideband
              | _ -> failwith "unknown mode"),
        f
          "Encoding mode, one of \"narrowband\", \"wideband\" or \
           \"ultra-wideband\". Default: %s"
          (string_of_mode !mode) );
      ( "--frame_per_packet",
        Arg.Int (fun b -> fpp := b),
        f "Frames per Ogg packet. Default: %i" !fpp );
      ( "--vbr",
        Arg.Unit (fun _ -> vbr := true),
        f "Encode in vbr mode. Default: %b" !vbr );
      ( "--quality",
        Arg.Int (fun b -> quality := b),
        f "Encoding bitrate, in Kbps. Default: %i" !quality );
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
  let ibits = input_short ic in
  if input_string ic 4 <> "data" then invalid_arg "No data tag";
  let _ = input_int ic in
  (* datalen *)
  let fos buf =
    let len = String.length buf / (2 * channels) in
    let ans = Array.init channels (fun _ -> Array.make len 0) in
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
        ans.(c).(i) <- max (-32768) (min 32767 n)
      done
    done;
    ans
  in
  let enc = Encoder.init !mode !fpp in
  if not !vbr then Encoder.set enc SPEEX_SET_QUALITY !quality
  else Encoder.set enc SPEEX_SET_VBR_QUALITY !quality;
  Encoder.set enc SPEEX_SET_SAMPLING_RATE infreq;
  let ivbr = if !vbr then 1 else 0 in
  Encoder.set enc SPEEX_SET_VBR ivbr;
  let fsize = Encoder.get enc SPEEX_GET_FRAME_SIZE in
  let os = Ogg.Stream.create () in
  let header =
    Header.init ~rate:infreq ~nb_channels:channels ~mode:!mode ~vbr:!vbr
      ~frames_per_packet:!fpp ()
  in
  Header.encode_header header [] os;
  let s_o_f (h, b) = h ^ b in
  let flush s =
    let rec f v =
      try
        let v = v ^ s_o_f (Ogg.Stream.flush_page s) in
        f v
      with Ogg.Not_enough_data -> v
    in
    f ""
  in
  output_string oc (flush os);
  let start = Unix.time () in
  let smode =
    match !mode with
      | Narrowband -> "narrowband"
      | Wideband -> "wideband"
      | Ultra_wideband -> "ultra-wideband"
  in
  Printf.printf "Input detected: PCM WAVE %d channels, %d Hz, %d bits\n%!"
    channels infreq ibits;
  Printf.printf
    "Encoding to: SPEEX %d channels, %d Hz, %s, VBR: %s\nPlease wait...\n%!"
    channels infreq smode (string_of_bool !vbr);
  begin try
    while true do
      let buflen = 2 * fsize * channels in
      let buf = Bytes.create buflen in
      let feed () =
        really_input ic buf 0 buflen;
        let buf = Bytes.to_string buf in
        let fbuf = fos buf in
        assert (Array.length fbuf.(0) = fsize);
        fbuf
      in
      let h, v =
        if channels = 1 then (
          let feed () =
            let frame = feed () in
            frame.(0)
          in
          if !float then (
            let feed () = Array.map float_of_int (feed ()) in
            Encoder.encode_page enc os feed)
          else Encoder.encode_page_int enc os feed)
        else if !float then (
          let feed () =
            Array.map (fun x -> Array.map float_of_int x) (feed ())
          in
          Encoder.encode_page_stereo enc os feed)
        else Encoder.encode_page_int_stereo enc os feed
      in
      output_string oc (h ^ v)
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
