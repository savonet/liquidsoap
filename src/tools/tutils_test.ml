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

open Dtools

let () =
  Conf.set_string "log.file" "/dev/null";
  Conf.set_bool "log.stdout" true

let log = Log.log ~label:"log" 3
let stop = ref false
let lock = Tutils.Mutex.create "foo"

let p s () =
  log (s ^ " runs");
  if s = "2" then Mutex.unlock lock;
  while not !stop do
    Thread.delay 1.
  done;
  log (s ^ " quits")

let main () =
  log "Hi!";
  ignore (Tutils.create (p "1") () "boule");
  ignore (Tutils.create (p "2") () "bill");
  Mutex.lock lock;
  Thread.delay 1. stop := true;
  log "Waiting...";
  Tutils.join_all_no_timeout ();
  log "Finished"

let () = Init.init main
