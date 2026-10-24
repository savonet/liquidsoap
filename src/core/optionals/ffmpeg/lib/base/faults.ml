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

let max_consecutive_faults = 100
let fault_log_interval = 10.
let fault_retry_delay = 5.

type kind_log = { mutable last_log : float; mutable suppressed : int }

type t = {
  log : Log.t;
  mutable count : int;
  kinds : (string, kind_log) Hashtbl.t;
}

let create log = { log; count = 0; kinds = Hashtbl.create 4 }

let report t ~kind message =
  let now = Unix.gettimeofday () in
  match Hashtbl.find_opt t.kinds kind with
    | None ->
        Hashtbl.replace t.kinds kind { last_log = now; suppressed = 0 };
        t.log#important "%s" message
    | Some k when now -. k.last_log >= fault_log_interval ->
        t.log#important "%s (%d similar since the last report)" message
          k.suppressed;
        k.last_log <- now;
        k.suppressed <- 0
    | Some k -> k.suppressed <- k.suppressed + 1

let uncounted t ~kind message = report t ~kind message

let item t ~kind message =
  report t ~kind message;
  t.count <- t.count + 1;
  if t.count >= max_consecutive_faults then (
    t.count <- 0;
    `Source_fault)
  else `Item_fault

let delivered t = t.count <- 0
let count t = t.count
