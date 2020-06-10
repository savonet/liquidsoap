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

(* Virtual class to keep track of a source's latest metadata *)
class virtual source =
  object (self)
    val mutable latest_metadata = Hashtbl.create 0

    method virtual private on_new_metadata : unit

    method private save_latest_metadata frame =
      let compare x y = -compare (fst x) (fst y) in
      let l = List.sort compare (Frame.get_all_metadata frame) in
      if List.length l > 0 then (
        latest_metadata <- Hashtbl.copy (snd (List.hd l));
        self#on_new_metadata )

    method private clear_latest_metadata = latest_metadata <- Hashtbl.create 0
  end
