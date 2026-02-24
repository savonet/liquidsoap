(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

open Mm

(** Decode raw data *)

open Extralib

let log = Log.make ["decoder"; "raw"]

(** {1 Generic decoder} *)

exception End_of_stream

(* TODO: some code should be shared with wav decoder, and possibly others. *)

type format = {
  format : [ `S8 | `S16LE | `F32LE ];
  channels : int;
  interleaved : bool;
  samplerate : int;
}

(** Bytes per sample. *)
let sample_size fmt =
  match fmt.format with `S8 -> 1 | `S16LE -> 2 | `F32LE -> 4

let create ~format input =
  (* TODO: can we handle interleaved with generators? I don't think so... *)
  assert (format.interleaved = true);
  let sample_size = sample_size format in
  let channels = format.channels in
  let bytes_to_get = sample_size * channels * Utils.buflen in
  let converter src =
    let len = String.length src / (sample_size * channels) in
    let dst = Audio.create channels len in
    let sample =
      let pos = ref 0 in
      match format.format with
        | `S8 ->
            fun () ->
              let ans = int_of_char src.[!pos] in
              let ans = if ans > 128 then ans - 256 else ans in
              incr pos;
              float ans /. 128.
        | `S16LE ->
            fun () ->
              let ans =
                int_of_char src.[!pos] + (int_of_char src.[!pos + 1] lsl 8)
              in
              let ans = if ans > 32768 then ans - 65536 else ans in
              pos := !pos + 2;
              float ans /. 32768.
        | `F32LE ->
            (* TODO: handle endianness *)
            fun () ->
              let ans = ref Int32.zero in
              for i = 3 downto 0 do
                ans := Int32.shift_left !ans 8;
                ans :=
                  Int32.add !ans (Int32.of_int (int_of_char src.[!pos + i]))
              done;
              pos := !pos + sample_size;
              Int32.float_of_bits !ans
    in
    for i = 0 to len - 1 do
      for c = 0 to channels - 1 do
        dst.(c).(i) <- sample ()
      done
    done;
    dst
  in
  let buf = Bytes.create bytes_to_get in
  let decoder buffer =
    let bytes = input.Decoder.read buf 0 bytes_to_get in
    if bytes = 0 then raise End_of_stream;
    let content = converter (Bytes.sub_string buf 0 bytes) in
    buffer.Decoder.put_pcm ~samplerate:format.samplerate content
  in
  (* TODO *)
  let seek _ = 0 in
  { Decoder.decode = decoder; seek; eof = (fun _ -> ()); close = (fun _ -> ()) }

(* The mime types are inspired of GStreamer's convention. See
   http://gstreamer.freedesktop.org/data/doc/gstreamer/head/pwg/html/section-types-definitions.html
   For instance: audio/x-raw,format=F32LE,channels=2,layout=interleaved,rate=44100 *)
(* TODO: proper parser? *)
let parse_mime m =
  let ans =
    ref
      { format = `F32LE; channels = 2; interleaved = true; samplerate = 44100 }
  in
  try
    let m = String.split_char ',' m in
    if m = [] || List.hd m <> "audio/x-raw" then raise Exit;
    let m = List.tl m in
    let m =
      List.map
        (fun lv ->
          let lv = String.split_char '=' lv in
          match lv with [l; v] -> (l, v) | _ -> raise Exit)
        m
    in
    List.iter
      (fun (l, v) ->
        match l with
          | "format" ->
              let format =
                List.assoc v [("S8", `S8); ("S16LE", `S16LE); ("F32LE", `F32LE)]
              in
              ans := { !ans with format }
          | "channels" ->
              let channels = int_of_string v in
              ans := { !ans with channels }
          | "layout" ->
              let interleaved =
                if v = "interleaved" then true
                else if v = "non-interleaved" then false
                else raise Exit
              in
              ans := { !ans with interleaved }
          | "rate" | "samplerate" ->
              let samplerate = int_of_string v in
              ans := { !ans with samplerate }
          | _ -> failwith ("Unknown property: " ^ l))
      m;
    Some !ans
  with _ -> None

let () =
  Plug.register Decoder.decoders "raw audio" ~doc:"Decode audio/x-raw."
    {
      Decoder.priority = (fun () -> 1);
      file_extensions = (fun () -> None);
      mime_types = (fun () -> Some ["audio/x-raw"]);
      file_type = (fun ~metadata:_ ~ctype:_ _ -> None);
      file_decoder = None;
      stream_decoder =
        Some
          (fun ~ctype:_ mime ->
            let format = Option.get (parse_mime mime) in
            create ~format);
    }
