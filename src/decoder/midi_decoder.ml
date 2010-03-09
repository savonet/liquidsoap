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

(** Read MIDI files.
  * The metadata support is TODO. *)

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

(** Read midi header. *)
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
        log#f 4 "Ticks per quarter: %d" division;
        Midi.Ticks_per_quarter division
      )
    else
      let frames = (division lsr 8) land 0x7f in
      let ticks = division land 0xff in
        log#f 4 "SMPTE: %d * %d" frames ticks;
        Midi.SMPTE (frames, ticks)
  in
    if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
      (
        log#f 4 "Invalid header (%s, %d, %d)" id len fmt;
        raise Invalid_header;
      );
    log#f 4 "Format: %d (%d tracks)" fmt tracks;
    tracks, division

(** Read a midi track. *)
let read_track fd =
  let id = read_id fd in
  let len = read_long fd in
  log#f 4 "Reading track %s (len: %d)" id len;
  if id <> "MTrk" then raise Invalid_header;
  let data = String.create len in
  let r = Utils.really_read fd data 0 len in
  if r <> len then log#f 4 "Read %d instead of %d" r len;
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
              Some chan,
              if v = 0 then
                (* I have seen notes at 0. used as note off...... *)
                Midi.Note_off (n, 0.)
              else
                Midi.Note_on (n, float v /. 127.)
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
                            log#f 6 "Tempo: %d µs per quarter" t;
                            None, Midi.Tempo t
                      | 0x58 (* Time signature *) ->
                          assert (len = 4);
                          (* numerator,
                           * denominator,
                           * ticks in a metronome click,
                           * 32nd notes to the quarter note *)
                          let n = get_byte () in
                          let d = get_byte () in
                          let c = get_byte () in
                          let b = get_byte () in
                            None, Midi.Time_signature (n, d, c, b)
                      | 0x59 (* Key signature *) ->
                          assert (len = 2);
                          let sf = get_byte () in (* sharps / flats *)
                          let m = get_byte () in (* minor? *)
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
  log#f 4 "Decoding %S..." file;
  let fd = Unix.openfile file [Unix.O_RDONLY] 0o644 in
  let closed = ref false in

  let close () =
    assert (not !closed);
    closed := true;
    Unix.close fd
  in
  let close_on_err f x =
    try f x with e ->
      log#f 4 "Closing on error: %s." (Printexc.to_string e);
      close (); raise e
  in

  let ntracks, division = close_on_err read_header fd in
  let tracks = close_on_err (Array.init ntracks) (fun _ -> read_track fd) in
    (* We don't need to access the file anymore. *)
    close ();
    (* Merge all tracks. *)
    let track =
      let find_min () =
        let ans = ref None in
          for c = 0 to Array.length tracks - 1 do
            match tracks.(c) with
              | [] -> ()
              | (d,_)::_ ->
                  match !ans with
                    | None ->
                        ans := Some (d, c)
                    | Some (d',_) ->
                        if d < d' then ans := Some (d, c)
          done;
          match !ans with
            | Some (d, c) -> d,c
            | None -> raise Not_found
      in
      let ans = ref [] in
        try
          while true do
            let d,c = find_min () in
              ans := (List.hd tracks.(c)) :: !ans;
              tracks.(c) <- List.tl tracks.(c);
              Array.iteri
                (fun n t ->
                   if n <> c && t <> [] then
                     let d',e = List.hd t in
                       tracks.(n) <- (d'-d,e)::(List.tl t)
                ) tracks
          done;
          assert false
        with
          | Not_found -> List.rev !ans
    in
    (* Convert delta-times in delta-liquidsoap-ticks. *)
    let track =
      let tempo = ref 125000 in
        List.map
          (fun (d,(c,e)) ->
             let d = Mutils.ticks_of_delta division !tempo d in
               (
                 match e with
                   | Midi.Tempo t ->
                       tempo := t
                   | _ -> ()
               );
               (d,(c,e))
          ) track
    in
    (* Filling function. *)
    let track = ref track in
    let warn_channels = ref true in
    let fill buf =
      let m = MFrame.content buf 0 in
      MFrame.clear buf;
      let buflen = MFrame.size () in
      let offset_in_buf = ref 0 in
        while !track <> [] && !offset_in_buf < buflen do
          let d,(c,e) = List.hd !track in
            offset_in_buf := !offset_in_buf + d;
            if !offset_in_buf < buflen then begin
              track := List.tl !track;
              match c with
               | Some c ->
                   (* Filter out relevant events. *)
                   begin match e with
                     | Midi.Note_on _
                     | Midi.Note_off _
                     | Midi.Control_change _ ->
                         begin try
                           m.(c) := !(m.(c))@[!offset_in_buf, e]
                         with
                           | Invalid_argument _ ->
                               if !warn_channels then begin
                                   log#f 3 "Event on channel %d \
                                     will be ignored, increase \
                                     frame.midi.channels (this message \
                                     is displayed only once)." c;
                                   warn_channels := false
                                 end
                         end
                       | _ -> () (* TODO *)
                   end
                | None -> () (* TODO *)
            end else
              track := (!offset_in_buf - buflen,(c,e))::(List.tl !track)
        done;
        if !track = [] then
          MFrame.add_break buf 0
        else
          MFrame.add_break buf (MFrame.size ());
        0
    in
      { Decoder.fill = fill ; Decoder.close = fun () -> () }

let () =
  Decoder.file_decoders#register "MIDI"
    (fun ~metadata filename kind ->
       (* Any number of MIDI channel is acceptable as the decoder
        * silently drops events on higher channels if needed. *)
       if kind.Frame.midi <> Frame.Zero then
           Some (fun () -> decoder filename)
       else
           None)
