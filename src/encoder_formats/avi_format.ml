(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

type t = {
  (* Samplerate is lazy in order to avoid forcing the evaluation of the
       samplerate at typing time, see #933. For channels this is pointless since
       we really need that for typing. *)
  samplerate: int Lazy.t;
  channels: int;
}

let to_string w =
  let samplerate = Lazy.force w.samplerate in
  Printf.sprintf "%%avi(samplerate=%d,channels=%d)" samplerate w.channels
