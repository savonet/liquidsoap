(*****************************************************************************

  Liquidsoap, a programmable stream generator.
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

type pcm = float array array

let create_buffer chans len =
  Array.init chans (fun _ -> Array.make len 0.)

let sub buf ofs len =
  if ofs = 0 && len = Array.length buf then
    buf
  else
    Array.map (fun a -> Array.sub a ofs len) buf

external caml_float_pcm_convert_le_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_le_byte" "caml_float_pcm_convert_le_native"

external caml_float_pcm_convert_be_byte : string -> int -> int ->
                      bool -> int -> bool ->
                      float ->
                      float array array -> int -> int
         = "caml_float_pcm_convert_be_byte" "caml_float_pcm_convert_be_native"

let resample_s16le
      src src_off len signed samplesize big_endian
      ratio dst dst_off =
  if big_endian then
    caml_float_pcm_convert_be_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off
  else
    caml_float_pcm_convert_le_byte
      src src_off len signed samplesize big_endian
      ratio dst dst_off

external to_s16le : float array array -> int -> int -> string -> int -> int
         = "caml_float_pcm_to_s16le"

let to_s16le_ni buf ofs len dst dst_ofs =
  let ans = ref 0 in
    for c = 0 to Array.length buf - 1 do
      ans := to_s16le [|buf.(c)|] ofs len dst.(c) dst_ofs;
    done;
    !ans

external from_s16le : float array array -> int -> string -> int -> int -> unit
         = "caml_float_pcm_from_s16le"

let from_s16le_ni dbuf dofs buf ofs len =
  for c = 0 to Array.length buf - 1 do
    from_s16le [|dbuf.(c)|] dofs buf.(c) ofs len
  done

external blit : float array -> int -> float array -> int -> int -> unit
     = "caml_float_array_blit"

let native_resample ratio inbuf offs len =
  if ratio = 1. then
        let outbuf = Array.make len 0. in
          blit inbuf offs outbuf 0 len;
          outbuf
  else
    let outlen = int_of_float (float len *. ratio) in
    let outbuf = Array.make outlen 0. in
      for i = 0 to outlen - 1 do
        let inidx = min (int_of_float (float i /. ratio)) (len - 1) in
          outbuf.(i) <- inbuf.(inidx + offs)
      done;
      outbuf

(* Sound processing *)

let multiply a off len c =
  for i = 0 to Array.length a - 1 do
    let a_i = a.(i) in
      for j = off to off + len - 1 do
        a_i.(j) <- c *. a_i.(j)
      done
  done

let blankify a off len =
  for i = 0 to Array.length a - 1 do
    let a_i = a.(i) in
      for j = off to off + len - 1 do
        a_i.(j) <- 0.
      done
  done

let add dst dst_off src src_off len =
  for i = 0 to Array.length dst - 1 do
    let dst_i = dst.(i) in
    let src_i = src.(i) in
      for j = 0 to len - 1 do
        dst_i.(dst_off+j) <- dst_i.(dst_off+j) +. src_i.(src_off+j)
      done
  done

let substract y y_off x x_off len =
  for i = 0 to Array.length y - 1 do
    let x_i = x.(i) in
    let y_i = y.(i) in
      for j = 0 to len - 1 do
        y_i.(y_off+j) <- y_i.(y_off+j) -. x_i.(x_off+j)
      done
  done

let rms a off len =
  let ans = Array.create (Array.length a) 0. in
    for c = 0 to Array.length a - 1 do
      let a_c = a.(c) in
        for i = off to off + len - 1 do
          ans.(c) <- ans.(c) +. a_c.(i) *. a_c.(i)
        done;
      ans.(c) <- sqrt (ans.(c) /. (float len))
    done;
    ans

let copy (a:float array) = Array.copy a
