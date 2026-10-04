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

type interest = { read : bool; write : bool; except : bool }

external available : unit -> bool = "caml_pollset_available"
external backend_name : unit -> string = "caml_pollset_backend_name"
external native_create : unit -> Unix.file_descr = "caml_pollset_create"

external native_set : Unix.file_descr -> Unix.file_descr -> bool -> bool -> unit
  = "caml_pollset_set"

external native_remove : Unix.file_descr -> Unix.file_descr -> unit
  = "caml_pollset_remove"

external native_wait : Unix.file_descr -> float -> (Unix.file_descr * int) array
  = "caml_pollset_wait"

type impl = Native of Unix.file_descr | Select

type t = {
  mutex : Mutex.t;
  registry : (Unix.file_descr, interest) Hashtbl.t;
  impl : impl;
}

let read_flag = 1
let write_flag = 2
let except_flag = 4

let interest_of_flags flags =
  {
    read = flags land read_flag <> 0;
    write = flags land write_flag <> 0;
    except = flags land except_flag <> 0;
  }

(* Setting LIQ_POLLSET_BACKEND=select forces the fallback, so the platform that
   only has it is not the only place it runs. *)
let forced_select () = Sys.getenv_opt "LIQ_POLLSET_BACKEND" = Some "select"

let create () =
  let impl =
    if available () && not (forced_select ()) then Native (native_create ())
    else Select
  in
  { mutex = Mutex.create (); registry = Hashtbl.create 64; impl }

let backend t =
  match t.impl with Native _ -> backend_name () | Select -> "select"

let close t =
  Mutex.protect t.mutex (fun () ->
      Hashtbl.reset t.registry;
      match t.impl with
        | Native set -> ( try Unix.close set with _ -> ())
        | Select -> ())

let set t fd interest =
  Mutex.protect t.mutex (fun () ->
      Hashtbl.replace t.registry fd interest;
      match t.impl with
        | Native set -> native_set set fd interest.read interest.write
        | Select -> ())

let remove t fd =
  Mutex.protect t.mutex (fun () ->
      Hashtbl.remove t.registry fd;
      match t.impl with Native set -> native_remove set fd | Select -> ())

let mem t fd = Mutex.protect t.mutex (fun () -> Hashtbl.mem t.registry fd)

(* Building the lists under the lock and releasing it before the syscall means a
   registration made while we wait lands on the next round, which is what the
   caller's wake-up descriptor is for. *)
let select_wait t timeout =
  let r, w, x =
    Mutex.protect t.mutex (fun () ->
        Hashtbl.fold
          (fun fd interest (r, w, x) ->
            ( (if interest.read then fd :: r else r),
              (if interest.write then fd :: w else w),
              if interest.except then fd :: x else x ))
          t.registry ([], [], []))
  in
  let r, w, x = Unix_utils.select r w x timeout in
  let fired = Hashtbl.create 16 in
  let add flag fd =
    let flags = try Hashtbl.find fired fd with Not_found -> 0 in
    Hashtbl.replace fired fd (flags lor flag)
  in
  List.iter (add read_flag) r;
  List.iter (add write_flag) w;
  List.iter (add except_flag) x;
  Hashtbl.fold
    (fun fd flags acc -> (fd, interest_of_flags flags) :: acc)
    fired []

let wait t ~timeout =
  match t.impl with
    | Native set ->
        Array.fold_left
          (fun acc (fd, flags) -> (fd, interest_of_flags flags) :: acc)
          [] (native_wait set timeout)
    | Select -> select_wait t timeout
