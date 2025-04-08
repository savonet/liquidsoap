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

module type Buffer = sig
  type t

  val create : int -> t
  val blit : t -> int -> t -> int -> int -> unit
end

module type R = sig
  type buffer
  type t

  val create : int -> t
  val read_space : t -> int
  val write_space : t -> int
  val read_advance : t -> int -> unit
  val write_advance : t -> int -> unit
  val read : t -> buffer -> int -> int -> unit
  val peek : t -> buffer -> int -> int -> unit
  val write : t -> buffer -> int -> int -> unit
  val transmit : t -> (buffer -> int -> int -> int) -> int
end

module Make (B : Buffer) = struct
  type buffer = B.t

  type t = {
    size : int;
    buffer : buffer;
    mutable rpos : int;  (** current read position *)
    mutable wpos : int;  (** current write position *)
  }

  let create size =
    {
      (* size + 1 so we can store full buffers, while keeping
         rpos and wpos different for implementation matters *)
      size = size + 1;
      buffer = B.create (size + 1);
      rpos = 0;
      wpos = 0;
    }

  let read_space t =
    if t.wpos >= t.rpos then t.wpos - t.rpos else t.size - (t.rpos - t.wpos)

  let write_space t =
    if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
    else t.rpos - t.wpos - 1

  let read_advance t n =
    assert (n <= read_space t);
    if t.rpos + n < t.size then t.rpos <- t.rpos + n
    else t.rpos <- t.rpos + n - t.size

  let write_advance t n =
    assert (n <= write_space t);
    if t.wpos + n < t.size then t.wpos <- t.wpos + n
    else t.wpos <- t.wpos + n - t.size

  let peek t buff off len =
    assert (len <= read_space t);
    let pre = t.size - t.rpos in
    let extra = len - pre in
    if extra > 0 then (
      B.blit t.buffer t.rpos buff off pre;
      B.blit t.buffer 0 buff (off + pre) extra)
    else B.blit t.buffer t.rpos buff off len

  let read t buff off len =
    peek t buff off len;
    read_advance t len

  let write t buff off len =
    assert (len <= write_space t);
    let pre = t.size - t.wpos in
    let extra = len - pre in
    if extra > 0 then (
      B.blit buff off t.buffer t.wpos pre;
      B.blit buff (off + pre) t.buffer 0 extra)
    else B.blit buff off t.buffer t.wpos len;
    write_advance t len

  let transmit t f =
    if t.wpos = t.rpos then 0
    else (
      let len0 =
        if t.wpos >= t.rpos then t.wpos - t.rpos else t.size - t.rpos
      in
      let len = f t.buffer t.rpos len0 in
      assert (len <= len0);
      read_advance t len;
      len)
end

module Make_ext (B : Buffer) = struct
  module R = Make (B)

  type buffer = R.buffer
  type t = { mutable ringbuffer : R.t }

  let prepare buf len =
    if R.write_space buf.ringbuffer >= len then buf.ringbuffer
    else (
      let rb = R.create (R.read_space buf.ringbuffer + len) in
      while R.read_space buf.ringbuffer <> 0 do
        ignore
          (R.transmit buf.ringbuffer (fun buf ofs len ->
               R.write rb buf ofs len;
               len))
      done;
      buf.ringbuffer <- rb;
      rb)

  let peek rb = R.peek rb.ringbuffer
  let read rb = R.read rb.ringbuffer

  let write rb buf ofs len =
    let rb = prepare rb len in
    R.write rb buf ofs len

  let transmit rb = R.transmit rb.ringbuffer
  let read_space rb = R.read_space rb.ringbuffer
  let write_space rb = R.write_space rb.ringbuffer
  let read_advance rb = R.read_advance rb.ringbuffer
  let write_advance rb = R.write_advance rb.ringbuffer
  let create len = { ringbuffer = R.create len }
end
