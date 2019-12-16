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

open Extralib

let word n =
  let s = Bytes.create 2 in
  Bytes.set s 0 (char_of_int (n land 0xff));
  Bytes.set s 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.unsafe_to_string s

let dword n =
  let s = Bytes.create 4 in
  Bytes.set s 0 (char_of_int (n land 0xff));
  Bytes.set s 1 (char_of_int ((n land 0xff00) lsr 8));
  Bytes.set s 2 (char_of_int ((n land 0xff0000) lsr 16));
  Bytes.set s 3 (char_of_int ((n land 0x7f000000) lsr 24));
  Bytes.unsafe_to_string s

let chunk id data =
  let n = String.length data in
  let ans = id ^ dword n ^ data in
  if n mod 2 = 0 then ans else ans ^ "\000"

(* Audio in 16LE *)
let audio_chunk b = chunk "01wb" b

(* Video in RGB. *)
let video_chunk b = chunk "00db" b

(* TODO: this could be merged with the above with low cost *)

(* The original data is cleared. *)
let chunk_strings id data =
  let n = Strings.Mutable.length data in
  let ans = Strings.of_list [id; dword n] in
  let ans = Strings.append ans (Strings.Mutable.to_strings data) in
  if n mod 2 <> 0 then Strings.add ans "\000" else ans

let audio_chunk_strings b = chunk_strings "01wb" b
let video_chunk_strings b = chunk_strings "00db" b
let list = chunk "LIST"

let header ~channels ~samplerate () =
  (* Writing in two steps because 0xffffffff cannot be represented on 32 bits
     architectures. *)
  let dword_max () = word 0xffff ^ word 0xffff in
  let video_rate = Lazy.force Frame.video_rate in
  let width = Lazy.force Frame.video_width in
  let height = Lazy.force Frame.video_height in
  let avi_header =
    chunk "avih"
      ( dword (1000000 / video_rate) (* microsec per frame *)
      ^ dword 0 (* maximum bytes per second *)
      ^ dword 0 (* reserved *)
      ^ dword 0x0100 (* flags (interleaved) *)
      ^ dword_max () (* number of frames *)
      ^ dword 0 (* initial frame *)
      ^ dword 2 (* number of streams *)
      ^ dword 0 (* suggested buffer size *)
      ^ dword width (* width *) ^ dword height (* height *)
      ^ dword 0 (* reserved *) ^ dword 0 (* reserved *)
      ^ dword 0 (* reserved *) ^ dword 0 )
    (* reserved *)
  in
  let video_header =
    let stream_header =
      chunk "strh"
        ( "vids" (* stream type *)
        ^ dword 0x30323449 (* fourcc (codec) *)
        ^ dword 0 (* flags *) ^ word 0
        (* priority *)
        ^ word 0 (* language *)
        ^ dword 0 (* initial frames *)
        ^ dword 1 (* scale *)
        ^ dword video_rate (* rate *)
        ^ dword 0 (* start time *)
        ^ dword_max () (* stream length *)
        ^ dword 0 (* suggested buffer size *)
        ^ dword_max () (* quality *) ^ dword 0 (* sample size *)
        ^ word 0 (* left *) ^ word 0
        (* top *)
        ^ word width (* right *)
        ^ word height )
      (* bottom *)
    in
    let stream_format =
      (* see BITMAPINFO *)
      chunk "strf"
        ( dword 40 (* size of this structure *)
        ^ dword width (* width *) ^ dword height (* height *)
        ^ word 1 (* panes *) ^ word 12 (* depth *)
        ^ dword 0x30323449 (* codec: I420 *)
        ^ dword (width * height * 6 / 4) (* image size *)
        ^ dword 0 (* pixels / x meter *)
        ^ dword 0 (* pixels / y meter *)
        ^ dword 0 (* colors used *) ^ dword 0 )
      (* important colors *)
    in
    list ("strl" ^ stream_header ^ stream_format)
  in
  let audio_header =
    let stream_header =
      chunk "strh"
        ( "auds" (* stream type *) ^ dword 0
        (* stream *)
        ^ dword 0 (* flags *)
        ^ word 0 (* priority *) ^ word 0 (* language *)
        ^ dword 0 (* initial frames *)
        ^ dword 1 (* scale *)
        ^ dword samplerate (* rate *)
        ^ dword 0 (* start time *)
        ^ dword_max () (* stream length *)
        ^ dword 0 (* suggested buffer size *)
        ^ dword_max () (* quality *)
        ^ dword (2 * channels) (* sample size *)
        ^ word 0 (* left *) ^ word 0
        (* top *)
        ^ word 0
        (* right *)
        ^ word 0 )
      (* bottom *)
    in
    let stream_format =
      chunk "strf"
        ( word 1 (* stream type (PCM) *)
        ^ word channels (* channels *)
        ^ dword samplerate (* rate *)
        ^ dword (2 * channels * samplerate) (* byte rate *)
        ^ word (2 * channels) (* block align *)
        ^ word 16 (* bits per sample *)
        ^ word 0 )
      (* size of extra information *)
    in
    list ("strl" ^ stream_header ^ stream_format)
  in
  let headers = list ("hdrl" ^ avi_header ^ video_header ^ audio_header) in
  let info =
    let producer = chunk "ISFT" Configure.vendor in
    list ("INFO" ^ producer)
  in
  "RIFF"
  ^ dword_max () (* file size *)
  ^ "AVI " ^ headers ^ info ^ "LIST" ^ dword_max () ^ "movi"

module Read = struct
  let read n f =
    let s = Bytes.create n in
    let k = read_retry f s 0 n in
    if k <> n then raise End_of_file;
    Bytes.unsafe_to_string s

  let word f =
    let s = read 2 f in
    int_of_char s.[0] + (int_of_char s.[1] lsl 8)

  let dword f =
    let s = read 4 f in
    int_of_char s.[0]
    + (int_of_char s.[1] lsl 8)
    + (int_of_char s.[2] lsl 16)
    + (int_of_char s.[3] lsl 24)

  exception Invalid of string

  let must s b = if not b then raise (Invalid s)

  let rec chunk f =
    let tag = read 4 f in
    let len = dword f in
    (* Printf.printf "Read: %s (%d)\n%!" tag len; *)
    ( len + 8,
      match tag with
        | "LIST" ->
            let subtag = read 4 f in
            if subtag = "movi" then
              (* for obvious size reasons we stop parsing here *)
              `movi (len - 4)
            else (
              let rem = ref (len - 4) in
              let ll = ref [] in
              (* Printf.printf "<<\n%!"; *)
              while !rem <> 0 do
                if !rem < 0 then raise (Invalid "Wrong header size.");
                let l, c = chunk f in
                rem := !rem - l;
                ll := c :: !ll
              done;

              (* Printf.printf ">>\n%!"; *)
              `LIST (subtag, List.rev !ll) )
        | "avih" ->
            (* microsec_per_frame *)
            let _ = dword f in
            (* max_bytes_per_sec *)
            let _ = dword f in
            (* reserved *)
            let _ = read 4 f in
            let flags = dword f in
            must "Not interleaved." (flags land 0x0100 <> 0);

            (* interleaved *)
            (* total_frame *)
            let _ = dword f in
            (* init_frame *)
            let _ = dword f in
            (* nb_stream *)
            let _ = dword f in
            (* sug_buf_size *)
            let _ = dword f in
            let width = dword f in
            let height = dword f in
            let scale = dword f in
            must "Non-zero scale." (scale = 0);

            (* rate *)
            let _ = dword f in
            (* start *)
            let _ = dword f in
            (* length *)
            let _ = dword f in
            `avih (width, height)
        | "strh" ->
            let stream_type = read 4 f in
            must "Wrong strh length." (len = 56);
            let fourcc = dword f in
            if
              not
                ( stream_type <> "vids" || fourcc = 0 || fourcc = 0x52474218
                (* RGB24 *) || fourcc = 0x30323449 )
              (* I420 *)
            then (
              let err =
                Printf.sprintf "Wrong %s fourcc: 0x%x." stream_type fourcc
              in
              must err false );
            must "Wrong auds fourcc." (stream_type <> "auds" || fourcc = 1);
            let flags = dword f in
            must "Wrong strh flags." (flags = 0);

            (* priority *)
            let _ = word f in
            (* language *)
            let _ = word f in
            (* init_frames *)
            let _ = dword f in
            let scale = dword f in
            let rate = dword f in
            let fps = float rate /. float scale in
            (* start *)
            let _ = dword f in
            (* length *)
            let _ = dword f in
            (* buf_size *)
            let _ = dword f in
            (* quality *)
            let _ = dword f in
            (* sample_size *)
            let _ = dword f in
            (* left *)
            let _ = word f in
            (* top *)
            let _ = word f in
            (* right *)
            let _ = word f in
            (* bottom *)
            let _ = word f in
            `strh (stream_type, fourcc, fps)
        | "strf" ->
            let s = read len f in
            `strf s
        | "JUNK" ->
            let s = read len f in
            `JUNK s
        | "00dc" ->
            let s = read len f in
            (* TODO: other channels and audio frames too (first argument is channel
          number) *)
            `Frame (`Video, 0, s)
        | "00wb" | "01wb" ->
            let s = read len f in
            `Frame (`Audio, 0, s)
        | _ ->
            let s = read len f in
            `Other s )

  let chunk f = snd (chunk f)

  let headers f =
    must "Not a RIFF file." (read 4 f = "RIFF");

    (* filesize *)
    let _ = dword f in
    must "Not an AVI file." (read 4 f = "AVI ");
    let h = ref [] in
    try
      while true do
        let c = chunk f in
        h := c :: !h;
        match c with `movi _ -> raise Exit | _ -> ()
      done;
      assert false
    with Exit -> List.rev !h

  let headers_simple f =
    let headers = headers f in
    if List.length headers < 2 then raise (Invalid "Not enough headers.");
    let h =
      match List.hd headers with
        | `LIST ("hdrl", h) -> h
        | _ -> raise (Invalid "Does not begin with hdrl.")
    in
    let width, height =
      match List.hd h with
        | `avih (width, height) -> (width, height)
        | _ -> raise (Invalid "Does not begin with avih.")
    in
    let h = List.tl h in
    let streams = ref [] in
    List.iter
      (function
        | `LIST ("strl", l) -> (
            if List.length l < 2 then raise (Invalid "strl too short.");
            let strf =
              match List.hd (List.tl l) with
                | `strf s -> s
                | _ -> raise (Invalid "strf expected.")
            in
            let word s o = int_of_char s.[o] + (int_of_char s.[o + 1] lsl 8) in
            let dword s o =
              int_of_char s.[o]
              + (int_of_char s.[o + 1] lsl 8)
              + (int_of_char s.[o + 2] lsl 16)
              + (int_of_char s.[o + 3] lsl 24)
            in
            let word = word strf in
            let dword = dword strf in
            match List.hd l with
              | `strh (stream_type, fourcc, fps) ->
                  if stream_type = "vids" then (
                    (* Printf.printf "video: %dx%d@%f\n%!" width height fps; *)
                    let fourcc =
                      match fourcc with
                        | 0 | 0x52474218 -> `RGB24
                        | 0x30323449 -> `I420
                        | _ -> assert false
                    in
                    streams := `Video (fourcc, width, height, fps) :: !streams )
                  else if stream_type = "auds" then (
                    let codec = word 0 in
                    must "Wrong audio codec." (codec = 1 || codec = 255);
                    let channels = word 2 in
                    let sample_rate = dword 4 in
                    streams := `Audio (channels, sample_rate) :: !streams )
                  else raise (Invalid "Unhandled stream type.")
              | _ -> () )
        | _ -> ())
      h;
    let streams = List.rev !streams in
    match List.last headers with
      | `movi len -> (streams, len)
      | _ -> raise (Invalid "Does not contain movi.")
end
