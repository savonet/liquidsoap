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

type buffer = { buf : Bytes.t; cap : int }
type t = { state : buffer Atomic.t; tail : int Atomic.t }

let create ~capacity =
  {
    state = Atomic.make { buf = Bytes.create capacity; cap = capacity };
    tail = Atomic.make 0;
  }

let tail t = Atomic.get t.tail
let capacity t = (Atomic.get t.state).cap

(* The old buffer is left untouched, so a reader holding it still sees intact
   bytes. *)
let grow t ~tail len =
  let old = Atomic.get t.state in
  let fresh = { buf = Bytes.create (2 * len); cap = 2 * len } in
  let start = max 0 (tail - old.cap) in
  let rec copy ofs remaining =
    if remaining > 0 then begin
      let src = ofs mod old.cap and dst = ofs mod fresh.cap in
      let n = min remaining (min (old.cap - src) (fresh.cap - dst)) in
      Bytes.blit old.buf src fresh.buf dst n;
      copy (ofs + n) (remaining - n)
    end
  in
  copy start (tail - start);
  Atomic.set t.state fresh;
  fresh

let append t strings =
  let len = Strings.length strings in
  if len > 0 then begin
    let tail = Atomic.get t.tail in
    let b = Atomic.get t.state in
    let b = if len > b.cap then grow t ~tail len else b in
    let pos = tail mod b.cap in
    let first = min len (b.cap - pos) in
    Strings.blit (Strings.sub strings 0 first) b.buf pos;
    if first < len then
      Strings.blit (Strings.sub strings first (len - first)) b.buf 0;
    Atomic.set t.tail (tail + len)
  end

let read t ~ofs dst dst_ofs len =
  let b = Atomic.get t.state in
  let tail = Atomic.get t.tail in
  if ofs < tail - b.cap || ofs + len > tail then false
  else begin
    let pos = ofs mod b.cap in
    let first = min len (b.cap - pos) in
    Bytes.blit b.buf pos dst dst_ofs first;
    if first < len then Bytes.blit b.buf 0 dst (dst_ofs + first) (len - first);
    (* The writer only overwrites offsets older than its tail minus this
       buffer's capacity, so a wrap over the range shows in the tail. *)
    ofs >= Atomic.get t.tail - b.cap
  end
