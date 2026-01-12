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

type ('a, 'b) t = { m : Mutex.t; h : ('a, 'b) Hashtbl.t }

let create n = { m = Mutex.create (); h = Hashtbl.create n }
let replace { m; h } k v = Mutex_utils.mutexify m (Hashtbl.replace h k) v
let mem { m; h } k = Mutex_utils.mutexify m (Hashtbl.mem h) k
let find { m; h } k = Mutex_utils.mutexify m (Hashtbl.find h) k
let find_opt { m; h } k = Mutex_utils.mutexify m (Hashtbl.find_opt h) k
let remove { m; h } k = Mutex_utils.mutexify m (Hashtbl.remove h) k
let length { m; h } = Mutex_utils.mutexify m Hashtbl.length h
