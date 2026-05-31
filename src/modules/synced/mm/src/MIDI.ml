(*
 * Copyright 2011 The Savonet Team
 *
 * This file is part of ocaml-mm.
 *
 * ocaml-mm is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * ocaml-mm is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ocaml-mm; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * As a special exception to the GNU Library General Public License, you may
 * link, statically or dynamically, a "work that uses the Library" with a publicly
 * distributed version of the Library to produce an executable file containing
 * portions of the Library, and distribute that executable file under terms of
 * your choice, without any of the additional requirements listed in clause 6
 * of the GNU Library General Public License.
 * By "a publicly distributed version of the Library", we mean either the unmodified
 * Library as distributed by The Savonet Team, or a modified version of the Library that is
 * distributed under the conditions defined in clause 3 of the GNU Library General
 * Public License. This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU Library General Public License.
 *
 *)

open Mm_base
open Mm_audio

type division = Ticks_per_quarter of int | SMPTE of int * int

type event =
  | Note_off of Audio.Note.t * float
  | Note_on of
      Audio.Note.t
      * float (* Note on: note number (A4 = 69), velocity (between 0 and 1). *)
  | Aftertouch of int * float
  | Control_change of
      int * int (* TODO: specific type for common control changes *)
  | Patch of int
  | Channel_aftertouch of int
  | Pitch of int
  (* Meta-events *)
  | Sequence_number of int
  | Text of string
  | Copyright of string
  | Track_name of string
  | Instrument_name of string
  | Lyric of string
  | Marker of string
  | Cue of string
  | End_of_track
  | Tempo of int
  | Time_signature of int * int * int * int
  | Key_signature of int * bool
  | Custom of string

(* Tempo is in microseconds per quarter. *)
let samples_of_delta samplerate division tempo delta =
  match division with
    | Ticks_per_quarter tpq ->
        (* These computations sometimes overflow on 32 bits. *)
        let tpq = Int64.of_int tpq in
        let tempo = Int64.of_int tempo in
        let tps = Int64.of_int samplerate in
        let ten = Int64.of_int 1000000 in
        let delta = Int64.of_int delta in
        let ( * ) = Int64.mul in
        let ( / ) = Int64.div in
        Int64.to_int (delta * tempo / tpq * tps / ten)
    | SMPTE (fps, res) -> samplerate * delta / (fps * res)

(*
let delta_of_samples samplerate division tempo samples =
  match division with
    | Ticks_per_quarter tpq ->
      let tpq = Int64.of_int tpq in
      let tempo = Int64.of_int tempo in
      let samplerate = Int64.of_int samplerate in
      let ten = Int64.of_int 1000000 in
      let samples = Int64.of_int samples in
      let ( * ) = Int64.mul in
      let ( / ) = Int64.div in
      Int64.to_int (samples * samplerate * ten * tpq / tempo)
    | SMPTE (fps,res) ->
      (* TODO: could this overflow? *)
      (fps * res * samples) / samplerate
*)

let byte_of_float x =
  let clip x = max 0 (min 127 x) in
  char_of_int (clip (int_of_float (x *. 127.)))

let encode_event chan e =
  let s = Bytes.create 3 in
  let coi = char_of_int in
  let bof = byte_of_float in
  (match e with
    | Note_off (n, v) ->
        Bytes.set s 0 (coi ((0x8 lsl 4) + chan));
        Bytes.set s 1 (coi n);
        Bytes.set s 2 (bof v)
    | Note_on (n, v) ->
        Bytes.set s 0 (coi ((0x9 lsl 4) + chan));
        Bytes.set s 1 (coi n);
        Bytes.set s 2 (bof v)
    | _ ->
        (* TODO *)
        assert false);
  Bytes.to_string s

type buffer = {
  (* time is offset from the beginning of the buffer at given samplerate *)
  mutable data : (int * event) list;
  duration : int;
}

let create duration = { data = []; duration }
let duration buf = buf.duration

let copy b =
  let ans = create (duration b) in
  ans.data <- b.data;
  ans

let clear_all buf = buf.data <- []

let clear buf ofs len =
  if ofs = 0 && len = duration buf then clear_all buf
  else
    buf.data <- List.filter (fun (t, _) -> t < ofs || t >= ofs + len) buf.data

(* complement of clear *)
let extract buf ofs len =
  if not (ofs = 0 && len = duration buf) then
    buf.data <- List.filter (fun (t, _) -> ofs <= t && t < ofs + len) buf.data

let cmp te1 te2 = fst te1 - fst te2
let merge b1 b2 = b1.data <- List.merge cmp b1.data b2.data
let translate b d = b.data <- List.map (fun (t, e) -> (t + d, e)) b.data

let add b1 o1 b2 o2 len =
  let b2 = copy b2 in
  extract b2 o2 len;
  translate b2 (o1 - o2);
  merge b1 b2

let blit_all b1 b2 = b2.data <- b1.data

let blit b1 o1 b2 o2 len =
  if o1 = 0 && o2 = 0 && duration b1 = len && duration b2 = len then
    blit_all b1 b2
  else (
    let b1 = copy b1 in
    clear b2 o2 len;
    extract b1 o1 len;
    translate b1 (o2 - o1);
    merge b2 b1)

let insert b te =
  assert (fst te < duration b);
  b.data <- List.merge cmp b.data [te]

let data buf = buf.data

module Multitrack = struct
  type t = buffer array
  type buffer = t

  let channels buf = Array.length buf
  let duration buf = duration buf.(0)
  let create chans duration = Array.init chans (fun _ -> create duration)

  let clear ?channel buf ofs len =
    match channel with
      | None -> Array.iter (fun buf -> clear buf ofs len) buf
      | Some c -> clear buf.(c) ofs len
end

module IO = struct
  exception Invalid_header
  exception Invalid_data

  module Reader = struct
    class type t = object
      method read : int -> Multitrack.buffer -> int -> int -> int
      method close : unit
    end

    class virtual base =
      object (self)
        inherit IO.helper
        val mutable tracks = 0
        val mutable division = Ticks_per_quarter 0
        method private input_id = self#really_input 4
        method! private input_int = self#input_int_be
        method! private input_short = self#input_short_be

        (** Read midi header. *)
        method private read_header =
          (* Actual header reading. *)
          let id = self#input_id in
          let len = self#input_int in
          let fmt = self#input_short in
          let track_nb = self#input_short in
          let div = self#input_short in
          let div =
            if div land 0x8000 = 0 then
              (* Delta-time ticks per quarter *)
              Ticks_per_quarter div
            else (
              let frames = (div lsr 8) land 0x7f in
              let ticks = div land 0xff in
              SMPTE (frames, ticks))
          in
          if id <> "MThd" || len <> 6 || (fmt <> 0 && fmt <> 1 && fmt <> 2) then
            raise Invalid_header;
          tracks <- track_nb;
          division <- div

        (** Read a midi track. *)
        method private decode_track_data data =
          let len = String.length data in
          let data = Array.init len (fun i -> int_of_char data.[i]) in
          let pos = ref 0 in
          let read_delta () =
            let ans = ref 0 in
            while data.(!pos) land 0x80 <> 0 do
              ans := (!ans lsl 7) + (data.(!pos) land 0x7f);
              incr pos;
              if !pos >= Array.length data then raise Invalid_data
            done;
            ans := (!ans lsl 7) + data.(!pos);
            incr pos;
            !ans
          in
          let status = ref 0 in
          (* for running status *)
          let read_event () =
            let get_byte () =
              if !pos >= Array.length data then raise Invalid_data;
              incr pos;
              data.(!pos - 1)
            in
            let get_text len =
              let ans = Bytes.create len in
              if !pos + len >= Array.length data then raise Invalid_data;
              for i = 0 to len - 1 do
                Bytes.set ans i (char_of_int data.(!pos + i))
              done;
              pos := !pos + len;
              Bytes.to_string ans
            in
            let advance len = pos := !pos + len in
            let command =
              if !pos >= Array.length data then raise Invalid_data;
              data.(!pos)
            in
            incr pos;
            let command =
              if command land 0x80 <> 0 then (
                status := command;
                command)
              else (
                decr pos;
                !status)
            in
            let cmd = (command lsr 4) land 0xf in
            let chan = command land 0xf in
            match cmd with
              | 8 ->
                  let n = get_byte () in
                  let v = get_byte () in
                  (Some chan, Note_off (n, float v /. 127.))
              | 9 ->
                  let n = get_byte () in
                  let v = get_byte () in
                  ( Some chan,
                    if v = 0 then
                      (* I have seen notes at 0. used as note off...... *)
                      Note_off (n, 0.)
                    else Note_on (n, float v /. 127.) )
              | 0xa ->
                  let n = get_byte () in
                  let v = get_byte () in
                  (Some chan, Aftertouch (n, float v /. 127.))
              | 0xb ->
                  let c = get_byte () in
                  let v = get_byte () in
                  (Some chan, Control_change (c, v))
              | 0xc ->
                  let p = get_byte () in
                  (Some chan, Patch p)
              | 0xd ->
                  let c = get_byte () in
                  (Some chan, Channel_aftertouch c)
              | 0xe ->
                  let l = get_byte () land 0x7f in
                  let h = get_byte () land 0x7f in
                  (Some chan, Pitch ((h lsl 7) + l))
              | _ -> (
                  match command with
                    | 0xf0 | 0xf7 ->
                        (* SysEx *)
                        let len = read_delta () in
                        advance len;
                        raise Not_found
                    | 0xff -> (
                        let cmd = get_byte () in
                        let len = read_delta () in
                        match cmd with
                          | 0 ->
                              if len <> 2 then raise Invalid_data;
                              let h = get_byte () in
                              let l = get_byte () in
                              (None, Sequence_number ((h lsl 8) + l))
                          | 1 -> (None, Text (get_text len))
                          | 2 -> (None, Copyright (get_text len))
                          | 3 -> (None, Track_name (get_text len))
                          | 4 -> (None, Instrument_name (get_text len))
                          | 5 -> (None, Lyric (get_text len))
                          | 6 -> (None, Marker (get_text len))
                          | 7 -> (None, Cue (get_text len))
                          | 0x2f (* End of track *) ->
                              if len <> 0 then raise Invalid_data;
                              raise Not_found
                          | 0x51 (* Tempo in microseconds per quarter note *) ->
                              if len <> 3 then raise Invalid_data;
                              let t1 = get_byte () in
                              let t2 = get_byte () in
                              let t3 = get_byte () in
                              let t = (t1 lsl 16) + (t2 lsl 8) + t3 in
                              (None, Tempo t)
                          | 0x58 (* Time signature *) ->
                              if len <> 4 then raise Invalid_data;
                              (* numerator,
                               * denominator,
                               * ticks in a metronome click,
                               * 32nd notes to the quarter note *)
                              let n = get_byte () in
                              let d = get_byte () in
                              let c = get_byte () in
                              let b = get_byte () in
                              (None, Time_signature (n, d, c, b))
                          | 0x59 (* Key signature *) ->
                              if len <> 2 then raise Invalid_data;
                              let sf = get_byte () in
                              (* sharps / flats *)
                              let m = get_byte () in
                              (* minor? *)
                              (None, Key_signature (sf, m <> 0))
                          | 0x54 (* SMPTE Offset *)
                          | 0x7f (* Sequencer-specific data *) ->
                              advance len;
                              raise Not_found
                          | _ ->
                              advance len;
                              Printf.printf "MIDI: unknown meta-event %x.\n%!"
                                cmd;
                              raise Not_found)
                    | _ ->
                        advance 1;
                        Printf.printf "MIDI: unknown command %x (pos: %d)\n%!"
                          command !pos;
                        raise Not_found)
          in
          let ans = ref [] in
          while !pos < len do
            try
              let d = read_delta () in
              let e = read_event () in
              ans := (d, e) :: !ans
            with Not_found -> ()
          done;
          List.rev !ans

        method private read_track =
          let id = self#input_id in
          let len = self#input_int in
          if id <> "MTrk" then raise Invalid_header;
          let data = self#really_input len in
          self#decode_track_data data
      end

    class of_file fname =
      object (self)
        inherit IO.Unix.rw ~read:true fname
        inherit IO.helper
        inherit! base
        val mutable track = []
        val mutable tempo = 500000

        initializer
          (* Read header. *)
          self#read_header;
          (* Read all tracks. *)
          let tracks = Array.init tracks (fun _ -> self#read_track) in
          (* Merge all tracks. *)
          let trk =
            let find_min () =
              let ans = ref None in
              for c = 0 to Array.length tracks - 1 do
                match tracks.(c) with
                  | [] -> ()
                  | (d, _) :: _ -> (
                      match !ans with
                        | None -> ans := Some (d, c)
                        | Some (d', _) -> if d < d' then ans := Some (d, c))
              done;
              match !ans with Some (d, c) -> (d, c) | None -> raise Not_found
            in
            let ans = ref [] in
            try
              while true do
                let d, c = find_min () in
                ans := List.hd tracks.(c) :: !ans;
                tracks.(c) <- List.tl tracks.(c);
                Array.iteri
                  (fun n t ->
                    if n <> c && t <> [] then (
                      let d', e = List.hd t in
                      tracks.(n) <- (d' - d, e) :: List.tl t))
                  tracks
              done;
              assert false
            with Not_found -> List.rev !ans
          in
          track <- trk

        (* We store here the track with delta-times in samples. TODO: this way of
           doing things is messy but simpler to implement *)
        val mutable track_samples = []
        val mutable track_samples_computed = false

        method private read_add sr buf ofs len =
          (* Compute track_samples if this has not been done yet. *)
          if not track_samples_computed then (
            let t = tempo in
            track_samples <-
              List.map
                (fun (d, (c, e)) ->
                  let d = samples_of_delta sr division tempo d in
                  (match e with Tempo t -> tempo <- t | _ -> ());
                  (d, (c, e)))
                track;
            tempo <- t;
            track_samples_computed <- true);
          let offset_in_buf = ref 0 in
          while track_samples <> [] && !offset_in_buf < len do
            let d, (c, e) = List.hd track_samples in
            offset_in_buf := !offset_in_buf + d;
            (match e with Tempo t -> tempo <- t | _ -> ());
            if !offset_in_buf < len then (
              track_samples <- List.tl track_samples;
              match c with
                | Some c -> (
                    (* Filter out relevant events. *)
                      match e with
                      | Note_on _ | Note_off _ | Control_change _ ->
                          if c < Array.length buf then
                            insert buf.(c) (!offset_in_buf + ofs, e)
                      | _ -> () (* TODO *))
                | None -> ()
              (* TODO *))
            else
              track_samples <-
                (!offset_in_buf - len, (c, e)) :: List.tl track_samples
          done;
          if track_samples <> [] then len else !offset_in_buf

        method read sr buf ofs len =
          Multitrack.clear buf ofs len;
          self#read_add sr buf ofs len

        method close = self#stream_close
      end
  end

  module Writer = struct
    class type t = object
      method put : int -> event -> unit
      method note_off : int -> int -> float -> unit
      method note_on : int -> int -> float -> unit
      method advance : int -> unit
      method close : unit
    end

    class to_file samplerate fname =
      (* frames per second *)
      let fps = 25 in
      (* ticks per frame *)
      let tpf = 40 in
      (* delta of samples *)
      let rec delta d =
        if d = 0 then ""
        else delta (d lsr 7) ^ String.make 1 (char_of_int (128 + (d land 127)))
      in
      let delta d =
        delta (d lsr 7) ^ String.make 1 (char_of_int (d land 127))
      in
      (* TODO: keep integer remainder in curdelta *)
      let delta d = delta (d * fps * tpf / samplerate) in
      object (self)
        inherit IO.Unix.rw ~write:true fname
        inherit IO.helper
        val mutable curdelta = 0
        val mutable datalen = 0
        method! private output_int = self#output_int_be
        method! private output_short = self#output_short_be

        initializer
          self#output "MThd";
          self#output_int 6;
          (* format *)
          self#output_short 0;
          (* tracks *)
          self#output_short 1;
          (* time division *)
          self#output_short ((((fps - 1) lxor 0xff) lsl 8) + tpf);
          (* Printf.printf "%dx%d: %x\n%!" fps tpf ((((fps-1) lxor 0xff) lsl 8) + tpf); *)
          (*
          self#output_byte (128 + fps);
          self#output_byte tpf;
        *)
          (* fist track *)
          self#output "MTrk";
          (* track length *)
          self#output_int 0

        method put chan e =
          let d = delta curdelta in
          let e = encode_event chan e in
          self#output d;
          self#output e;
          datalen <- datalen + String.length d + String.length e;
          curdelta <- 0

        method note_off chan n v = self#put chan (Note_off (n, v))
        method note_on chan n v = self#put chan (Note_on (n, v))
        method advance n = curdelta <- curdelta + n

        method close =
          self#stream_seek 18;
          self#output_int datalen;
          self#stream_close
      end
  end
end
