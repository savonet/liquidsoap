(*
 * Copyright 2003-2006 Savonet team
 *
 * This file is part of OCaml-mad.
 *
 * Ocaml-mad is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Ocaml-mad is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Ocaml-mad; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
  * Decode message encoded using private bits.
  *
  * @author Romain Beauxis
  *)

(* $Id$ *)

let src = ref ""
let usage = "usage: decode_msg source"
let last = ref (false, true, false)

let state () =
  let x, _, _ = !last in
  x

let started = ref false
let iob b = if b then 1 else 0

let get_bits mf char =
  let frame = Mad.get_frame_format mf in
  let cp = frame.Mad.copyright in
  let oi = frame.Mad.original in
  let pr = frame.Mad.private_bit in
  let lcp, _, _ = !last in
  last := (cp, oi, pr);
  if cp = not lcp then begin
    let char = (char lsl 1) + iob oi in
    (true, (char lsl 1) + iob pr)
  end
  else (false, char)

let is_sync mf =
  let frame = Mad.get_frame_format mf in
  let cp = frame.Mad.copyright in
  let oi = frame.Mad.original in
  let pr = frame.Mad.private_bit in
  let lcp, loi, lpr = !last in
  last := (cp, oi, pr);
  if !started && loi = not oi && lpr = not pr && cp = lcp then true
  else begin
    started := true;
    false
  end

let _ =
  Arg.parse []
    (let pnum = ref (-1) in
     fun s ->
       incr pnum;
       match !pnum with
         | 0 -> src := s
         | _ ->
             Printf.eprintf "Error: too many arguments\n";
             exit 1)
    usage;
  if !src = "" then (
    Printf.printf "%s\n" usage;
    exit 1);

  let synced = ref false in
  let msg_char = ref 0 in
  let bit_pos = ref 0 in
  let mf = Mad.openfile !src in
  (try
     while true do
       Mad.skip_frame mf;
       begin match !synced with
         | false when is_sync mf -> synced := true
         | false -> ()
         | true ->
             let incr, char = get_bits mf !msg_char in
             msg_char := char;
             if incr then bit_pos := !bit_pos + 2
       end;
       if !bit_pos > 7 then begin
         Printf.printf "%C%!" (char_of_int !msg_char);
         msg_char := 0;
         bit_pos := 0;
         synced := false
       end
     done;
     assert false
   with Mad.End_of_stream -> Mad.close mf);
  Printf.printf "\n"
