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

(** Helper functions for reading and writing. *)

exception Invalid_data

module Unix = struct
  (** To be inherited to read and write from files. *)
  class virtual rw ?(read = false) ?(write = false) fname =
    object
      val fd =
        let flag, perms =
          match (read, write) with
            | false, false -> assert false
            | true, false -> ([Unix.O_RDONLY], 0o644)
            | false, true -> ([Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC], 0o644)
            | true, true -> ([Unix.O_RDWR; Unix.O_CREAT; Unix.O_TRUNC], 0o644)
        in
        Unix.openfile fname flag perms

      method private stream_read buf ofs len = Unix.read fd buf ofs len

      method private stream_write buf ofs len =
        Unix.write fd (Bytes.of_string buf) ofs len

      method private stream_close = Unix.close fd
      method private stream_seek n = ignore (Unix.lseek fd n Unix.SEEK_SET)
      method private stream_cur_pos = Unix.lseek fd 0 Unix.SEEK_CUR
    end
end

class virtual helper =
  object (self)
    method virtual private stream_read : Bytes.t -> int -> int -> int

    method private input_once n =
      let buf = Bytes.create n in
      let n = self#stream_read buf 0 n in
      if n = Bytes.length buf then buf else Bytes.sub buf 0 n

    method private input n =
      let buf = self#input_once n in
      let buf = Bytes.to_string buf in
      let buflen = String.length buf in
      if buflen = n || buflen = 0 then buf else buf ^ self#input (n - buflen)

    method private really_input n =
      let buf = self#input n in
      if String.length buf <> n then raise Invalid_data;
      buf

    method private input_byte =
      let buf = self#really_input 1 in
      int_of_char buf.[0]

    (* TODO: use really_input instead of input_byte *)
    method private input_int_num_bytes n =
      let rec aux = function
        | 0 -> 0
        | n ->
            let b = self#input_byte in
            b + (256 * aux (n - 1))
      in
      aux n

    method private input_int = self#input_int_num_bytes 4
    method private input_short = self#input_int_num_bytes 2

    method private input_int_num_bytes_be n =
      let ans = ref 0 in
      let buf = self#really_input n in
      for i = 0 to n - 1 do
        ans := (256 * !ans) + int_of_char buf.[i]
      done;
      !ans

    method private input_int_be = self#input_int_num_bytes_be 4
    method private input_short_be = self#input_int_num_bytes_be 2
    method virtual private stream_write : string -> int -> int -> int

    method private output s =
      let len = String.length s in
      assert (self#stream_write s 0 len = len)

    method private output_num b n =
      let s = Bytes.create b in
      for i = 0 to b - 1 do
        Bytes.set s i (char_of_int ((n lsr (8 * i)) land 0xff))
      done;
      self#output (Bytes.to_string s)

    method private output_byte n = self#output_num 1 n
    method private output_short n = self#output_num 2 n
    method private output_int n = self#output_num 4 n

    method private output_num_be b n =
      let s = Bytes.create b in
      for i = 0 to b - 1 do
        Bytes.set s i (char_of_int ((n lsr (8 * (b - i - 1))) land 0xff))
      done;
      self#output (Bytes.to_string s)

    method private output_short_be n = self#output_num_be 2 n
    method private output_int_be n = self#output_num_be 4 n
  end
