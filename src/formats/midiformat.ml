(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2009 Savonet team

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
        (* Delta-time ticks per quarter *)
        log#f 5 "Ticks per quarter: %d" division;
        Midi.Ticks_per_quarter division
      )
    else
      let frames = (division lsr 8) land 0x7f in
      let ticks = division land 0xff in
        log#f 5 "SMPTE: %d * %d" frames ticks;
        Midi.SMPTE (frames, ticks)
  in
    if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
      (
        log#f 4 "Invalid header (%s, %d, %d)" id len fmt;
        raise Invalid_header;
      );
    log#f 5 "Format: %d (%d tracks)" fmt tracks;
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
  let status = ref 0 in (* for running status *)
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
    let advance len =
      pos := !pos + len
    in
    let command = data.(!pos) in
    incr pos;
    let command =
      if command land 0x80 <> 0 then
        (
          status := command;
          command
        )
      else
        (
          decr pos;
          !status
        )
    in
    let cmd = (command lsr 4) land 0xf in
    let chan = command land 0xf in
      match cmd with
        | 8 ->
            let n = get_byte () in
            let v = get_byte () in
              Some chan, Midi.Note_off (n, float v /. 127.)
        | 9 ->
            let n = get_byte () in
            let v = get_byte () in
              Some chan, Midi.Note_on (n, float v /. 127.)
        | 0xa ->
            let n = get_byte () in
            let v = get_byte () in
              Some chan, Midi.Aftertouch (n, float v /. 127.)
        | 0xb ->
            let c = get_byte () in
            let v = get_byte () in
              Some chan, Midi.Control_change (c, v)
        | 0xc ->
            let p = get_byte () in
              Some chan, Midi.Patch p
        | 0xd ->
            let c = get_byte () in
              Some chan, Midi.Channel_aftertouch c
        | 0xe ->
            let l = get_byte () land 0x7f in
            let h = get_byte () land 0x7f in
              Some chan, Midi.Pitch ((h lsl 7) + l)
        | _ ->
            match command with
              | 0xf0
              | 0xf7 ->
                  (* SysEx *)
                  let len = read_delta () in
                    advance len;
                    raise Not_found
              | 0xff ->
                  (
                  let cmd = get_byte () in
                  let len = read_delta () in
                    match cmd with
                      | 0 ->
                          assert (len = 2);
                          let h = get_byte () in
                          let l = get_byte () in
                            None, Midi.Sequence_number ((h lsl 8) + l)
                      | 1 ->
                          None, Midi.Text (get_text len)
                      | 2 ->
                          None, Midi.Copyright (get_text len)
                      | 3 ->
                          None, Midi.Track_name (get_text len)
                      | 4 ->
                          None, Midi.Instrument_name (get_text len)
                      | 5 ->
                          None, Midi.Lyric (get_text len)
                      | 6 ->
                          None, Midi.Marker (get_text len)
                      | 7 ->
                          None, Midi.Cue (get_text len)
                      | 0x2f (* End of track *) ->
                          assert (len = 0);
                          raise Not_found
                      | 0x51 (* Tempo in microseconds per quarter note *) ->
                          assert (len = 3);
                          let t1 = get_byte () in
                          let t2 = get_byte () in
                          let t3 = get_byte () in
                          let t = t1 lsl 16 + t2 lsl 8 + t3 in
                            ignore t;
                            None, Midi.Tempo t
                      | 0x58 (* Time signature *) ->
                          assert (len = 4);
                          let n = get_byte () in (* numerator *)
                          let d = get_byte () in (* denominator *)
                          let c = get_byte () in (* ticks in a metronome click *)
                          let b = get_byte () in (* 32nd notes to the quarter note *)
                            ignore (n, d, c, b);
                            None, Midi.Time_signature (n, d, c, b)
                      | 0x59 (* Key signature *) ->
                          assert (len = 2);
                          let sf = get_byte () in (* sharps / flats *)
                          let m = get_byte () in (* minor? *)
                            ignore (sf, m);
                            None, Midi.Key_signature (sf, m <> 0)
                      | 0x54 (* SMPTE Offset *)
                      | 0x7f (* Sequencer-specific data *) ->
                          advance len;
                          raise Not_found
                      | _ ->
                          advance len;
                          log#f 5 "Unknown meta-event %x" cmd;
                          raise Not_found
                  )
              | _ ->
                  advance 1;
                  log#f 5 "Unknown command %x (pos: %d)" command !pos;
                  raise Not_found
  in
  let ans = ref [] in
    while !pos < len do
      try
        let d = read_delta () in
        let e = read_event () in
        ans := (d, e)::!ans
      with
        | Not_found -> ()
    done;
    List.rev !ans

let decoder file =
  log#f 4 "Decoding %s..." file;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
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
    (* Convert delta-times in delta-liquidsoap-ticks. *)
    for n = 0 to ntracks - 1 do
      let tpq =
        match division with
          | Midi.Ticks_per_quarter tpq -> tpq
          | _ -> assert false
      in
      let tempo = ref 125000 in
        tracks.(n) <-
          List.map
            (fun (d,(c,e)) ->
               let d = (d * !tempo / tpq) * Fmt.ticks_per_second () / 1000000 in
               let d = d * 2 in (* TODO: remove this! *)
                 (
                   match e with
                     | Midi.Tempo t ->
                         tempo := t
                     | _ -> ()
                 );
                 (d,(c,e))
            ) tracks.(n)
    done;
    (* Merge all tracks. *)
    let track = ref tracks.(0) in (* TODO! *)
    (* Filling function. *)
    let fill buf =
      let m = MFrame.tracks buf in
      (* TODO: why do we have to do this here??? *)
      AFrame.blankify buf 0 (AFrame.size buf);
      m.(0) := [];
      let buflen = MFrame.size buf in
      let offset_in_buf = ref 0 in
        while !track <> [] && !offset_in_buf < buflen do
          let d,(c,e) = List.hd !track in
            (* Printf.printf "delta: %d\n%!" d; *)
            offset_in_buf := !offset_in_buf + d;
            if !offset_in_buf < buflen then
              (
                track := List.tl !track;
                match c with
                  | Some c ->
                      (
                        match e with
                          | Midi.Note_on _
                          | Midi.Note_off _ ->
                              (* Printf.printf "EVENT (chan %d)!\n%!" c; *)
                              let c = if c <> 10 then 0 else c in (* TODO: remove this *)
                              m.(c) := !(m.(c))@[!offset_in_buf, e]
                          | _ -> () (* TODO *)
                      )
                  | None -> () (* TODO *)
              )
            else
              track := (!offset_in_buf - buflen,(c,e))::(List.tl !track)
        done;
        MFrame.add_break buf (MFrame.size buf);
        0
    in
      { Decoder.fill = fill ; Decoder.close = fun () -> () }

let () =
  Decoder.formats#register "MID"
    (fun name -> try Some (decoder name) with _ -> None)

(* TODO *)
(*
let metadatas ~format file =
  []

let () = Request.mresolvers#register "MID" metadatas
*)
