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

type t = {
  size   : int ;
  buffer : float array array ;
  mutable rpos : int ;
  mutable wpos : int
}

(* rpos stores the current read position
 * wpos stores the current write position *)

let create chans size =
  {
    (* size + 1 so we can store full buffers, while keeping
       rpos and wpos different for implementation matters *)
    size = size + 1 ;
    buffer = Array.init chans (fun _ -> Array.create (size + 1) 0.) ;
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
    if extra > 0 then
      for c = 0 to Array.length t.buffer - 1 do
        Float_pcm.blit t.buffer.(c) t.rpos buff.(c) off pre;
        Float_pcm.blit t.buffer.(c) 0 buff.(c) (off + pre) extra
      done
    else
      for c = 0 to Array.length t.buffer - 1 do
        Float_pcm.blit t.buffer.(c) t.rpos buff.(c) off len
      done;
    read_advance t len

let write t buff off len =
  assert (len <= write_space t) ;
  let pre = t.size - t.wpos in
  let extra = len - pre in
    if extra > 0 then
      for c = 0 to Array.length t.buffer - 1 do
        Float_pcm.blit buff.(c) off t.buffer.(c) t.wpos pre;
        Float_pcm.blit buff.(c) (off + pre) t.buffer.(c) 0 extra
      done
    else
      for c = 0 to Array.length t.buffer - 1 do
        Float_pcm.blit buff.(c) off t.buffer.(c) t.wpos len
      done;
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

let ts m f =
  try
    Mutex.lock m;
    let ans = f () in
      Mutex.unlock m;
      ans
  with
    | e ->
        Mutex.unlock m;
        raise e

module TS =
struct
  type ts_t = t * Mutex.t

  type t = ts_t

  let create chans size =
    create chans size, Mutex.create ()

  let read_space (t,m) =
    ts m (fun () -> read_space t)

  let write_space (t,m) =
    ts m (fun () -> write_space t)

  let read_advance (t,m) n =
    ts m (fun () -> read_advance t n)

  let write_advance (t,m) n =
    ts m (fun () -> write_advance t n)

  let read (t,m) buf off len =
    ts m (fun () -> read t buf off len)

  let write (t,m) buf off len =
    ts m (fun () -> write t buf off len)

  let transmit (t,m) f =
    ts m (fun () -> transmit t f)
end
