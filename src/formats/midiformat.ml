(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2007 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

(** Read MIDI files. *)

exception Invalid_header

let log = Dtools.Log.make ["format";"midi"]

let read_id fd =
  let buf = String.create 4 in
    assert (Unix.read fd buf 0 4 = 4);
    buf

let read_nat n fd =
  let buf = String.create n in
  let ans = ref 0 in
    assert (Unix.read fd buf 0 n = n);
    for i = 0 to n - 1 do
      ans := !ans lsl 8 + int_of_char buf.[i]
    done;
    !ans

let read_long = read_nat 4
let read_short = read_nat 2

let read_header fd =
  let id = read_id fd in
  let len = read_long fd in
  let fmt = read_short fd in
  let tracks = read_short fd in
  let division = read_short fd in
  let division =
    if division land 0x8000 = 0 then
      (
        log#f 5 "Ticks per quarter: %d" division;
        Midi.Ticks_per_quarter division
      )
    else
      let frames = (division lsr 8) land 0x7f in
      let ticks = division land 0xff in
        Midi.SMPTE (frames, ticks)
  in
    if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
      (
        log#f 4 "Invalid header (%s, %d, %d)" id len fmt;
        raise Invalid_header;
      );
    log#f 5 "Tracks: %d" tracks;
    tracks, division

let read_track fd =
  let id = read_id fd in
  let len = read_long fd in
  log#f 5 "Reading track %s (len: %d)" id len;
  if id <> "MTrk" then raise Invalid_header;
  let data = String.create len in
  let r = Utils.really_read fd data 0 len in
  if r <> len then log#f 5 "Read %d instead of %d" r len;
  assert (r = len);
  let data = Array.init len (fun i -> int_of_char data.[i]) in
  let pos = ref 0 in
  let read_delta () =
    let ans = ref 0 in
      while data.(!pos) land 0x80 <> 0 do
        ans := !ans lsl 7 + (data.(!pos) land 0x7f);
        incr pos
      done;
      ans := !ans lsl 7 + data.(!pos);
      incr pos;
      !ans
  in
  let read_event () =
    let get_byte () =
      incr pos;
      data.(!pos - 1)
    in
    let get_text len =
      let ans = String.create len in
        for i = 0 to len - 1 do
          ans.[i] <- char_of_int data.(!pos + i)
        done;
        pos := !pos + len;
        ans
    in
    let cmd = (data.(!pos) lsr 4) land 0xf in
    let chan = data.(!pos) land 0xf in
    incr pos;
    let event =
      match cmd with
        | 8 ->
            let n = get_byte () in
            let v = get_byte () in
              Midi.Note_off (n, v)
        | 9 ->
            let n = get_byte () in
            let v = get_byte () in
              log#f 6 "Note on: %d %d" n v;
              Midi.Note_on (n, v)
        | 10 ->
            let n = get_byte () in
            let v = get_byte () in
              Midi.Aftertouch (n, v)
        | 11 ->
            let c = get_byte () in
            let v = get_byte () in
              Midi.Control_change (c, v)
        | 12 ->
            let p = get_byte () in
              Midi.Patch p
        | 13 ->
            let c = get_byte () in
              Midi.Channel_aftertouch c
        | 14 ->
            let l = get_byte () land 0x7f in
            let h = get_byte () land 0x7f in
              Midi.Pitch ((h lsl 7) + l)
        | 15 ->
            (
              let cmd = get_byte () in
                match cmd with
                  | 0 ->
                      assert (get_byte () = 2);
                      let h = get_byte () in
                      let l = get_byte () in
                        Midi.Sequence_number ((h lsl 8) + l)
                  | 1 ->
                      let len = get_byte () in
                        Midi.Text (get_text len)
                  | 2 ->
                      let len = get_byte () in
                        Midi.Copyright (get_text len)
                  | 3 ->
                      let len = get_byte () in
                        Midi.Track_name (get_text len)
                  | 4 ->
                      let len = get_byte () in
                        Midi.Instrument_name (get_text len)
                  | 5 ->
                      let len = get_byte () in
                        Midi.Lyric (get_text len)
                  | 6 ->
                      let len = get_byte () in
                        Midi.Marker (get_text len)
                  | 7 ->
                      let len = get_byte () in
                        Midi.Cue (get_text len)
                  | _ ->
                      log#f 5 "Unknown command 15,%d" cmd;
                      raise Not_found
            )
        | _ ->
            (* log#f 5 "Unknown command %d" cmd; *)
            raise Not_found
    in
      chan, event
  in
  let ans = ref [] in
    while !pos < len do
      try
        ans := (read_delta (), read_event ())::!ans
      with
        | Not_found -> ()
    done;
    List.rev !ans

let decoder file =
  log#f 4 "Decoding %s..." file;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0 in
  let closed = ref false in

  let close () =
    assert (not !closed);
    closed := true;
    Unix.close fd
  in
  let close_on_err f x =
    try f x with e -> log#f 5 "Closing on error: %s" (Printexc.to_string e); close (); raise e
  in

  let ntracks, division = close_on_err read_header fd in
  let tracks = close_on_err (Array.init ntracks) (fun _ -> read_track fd) in
    (* We don't need to access the file anymore. *)
    close ();
    log#f 5 "Read %d events" (List.length tracks.(0));
    let fill buf =
      1000 (* TODO *)
    in
      { Decoder.fill = fill ; Decoder.close = fun () -> () }

let () =
  Decoder.formats#register "MID"
    (fun name -> try Some (decoder name) with _ -> None)

(* TODO *)
let metadatas ~format file =
  []

let () = Request.mresolvers#register "MID" metadatas
