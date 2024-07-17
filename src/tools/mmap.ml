(*
  Copyright 2004 The Savonet team.

  This file is part of Liquidsoap.

  Liquidsoap is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License version 2, as
  published by the Free Software Foundation.

  Liquidsoap is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Liquidsoap; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(**
   Ugly hack to avoid copying buffer between the main process and the
   encoding/output processes.
   Thanks to Jacques Garrigue <garrigue@math.nagoya-u.ac.jp> for his
   help. See the thread beginning at
   http://caml.inria.fr/archives/200412/msg00076.html for details.

   @author Julien Cristau
*)

external mmap_anon : ('a, 'b) Bigarray.kind -> 'c Bigarray.layout ->
  bool -> int array -> ('a, 'b, 'c) Bigarray.Array1.t =
    "liquidsoap_map_anon"

(* we need to keep the bigarrays accessible, otherwise they are
   finalized and we get a SIGSEGV *)
let bigaref = ref []

let str_of_bigarray biga =
  (Obj.magic (snd (Obj.magic biga : Obj.t * int) + 2) : string)

let copy_string s biga =
  String.unsafe_blit s (-4) (str_of_bigarray biga) (-4)
    ((String.length s / 4 + 2)*4)

let mmap_string size =
  let biga = mmap_anon Bigarray.char Bigarray.c_layout true
    [|((size / 4 + 2)*4) + 10|] in
  let s = String.create size in
    copy_string s biga;
    bigaref := biga :: !bigaref;
    str_of_bigarray biga
