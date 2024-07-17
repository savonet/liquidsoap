(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2008 Savonet team

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

type t = {
  size   : int ;
  buffer : float array array ;
  mutable rpos : int ;
  mutable wpos : int
}

(* rpos stores the current read position
 * wpos stores the current write position *)

let create chans size =
  { size = size ;
    buffer = Array.init chans (fun _ -> Array.create size 0.) ;
    rpos = 0 ;
    wpos = 0
  }

let read_space t =
  if t.wpos >= t.rpos then (t.wpos - t.rpos)
  else t.size - (t.rpos - t.wpos)

let write_space t =
  if t.wpos >= t.rpos then t.size - (t.wpos - t.rpos) - 1
  else (t.rpos - t.wpos) - 1

let read_advance t n =
  assert (n <= read_space t) ;
  if t.rpos + n < t.size then t.rpos <- t.rpos + n
  else t.rpos <- t.rpos + n - t.size

let write_advance t n =
  assert (n <= write_space t) ;
  if t.wpos + n < t.size then t.wpos <- t.wpos + n
  else t.wpos <- t.wpos + n - t.size

let read t buff off len =
  assert (len <= read_space t) ;
  let pre = t.size - t.rpos in
  let extra = len - pre in
  begin match extra > 0 with
  | false ->
      for c = 0 to Array.length t.buffer - 1 do
        Array.blit t.buffer.(c) t.rpos buff.(c) off len
      done;
  | true ->
      for c = 0 to Array.length t.buffer - 1 do
        Array.blit t.buffer.(c) t.rpos buff.(c) off pre;
        Array.blit t.buffer.(c) 0 buff.(c) (off + pre) extra
      done;
  end;
  read_advance t len

let write t buff off len =
  assert (len <= write_space t) ;
  let pre = t.size - t.wpos in
  let extra = len - pre in
  begin match extra > 0 with
  | false ->
      for c = 0 to Array.length t.buffer - 1 do
        Array.blit buff.(c) off t.buffer.(c) t.wpos len
      done;
  | true ->
      for c = 0 to Array.length t.buffer - 1 do
        Array.blit buff.(c) off t.buffer.(c) t.wpos pre;
        Array.blit buff.(c) (off + pre) t.buffer.(c) 0 extra
      done;
  end;
  write_advance t len

let transmit t f =
  if t.wpos = t.rpos then 0 else
  let len0 =
    if t.wpos >= t.rpos then t.wpos - t.rpos
    else t.size - t.rpos
  in
  let len = f t.buffer t.rpos len0 in
  assert (len <= len0) ;
  read_advance t len ;
  len
