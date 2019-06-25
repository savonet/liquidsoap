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

open Extralib

(* Wrap synchronous read in a asynchronous context using a buffer. *)
module Async_read = struct
  exception Not_enough_data

  type t = {
    buf : Buffer.t;
    m   : Mutex.t;
    mutable offset: int
  }

  let init () =
    {buf    = Buffer.create 1024;
     m      = Mutex.create ();
     offset = 0}

  let add_bytes {buf;m} =
    Tutils.mutexify m (Buffer.add_bytes buf)

  let read t =
    Tutils.mutexify t.m (fun n ->
      if Buffer.length t.buf < n+t.offset then
        raise Not_enough_data;
      let ret = Buffer.sub t.buf t.offset n in
      t.offset <- t.offset + n;
      ret)

  let reset t =
    Tutils.mutexify t.m (fun () ->
      t.offset <- 0) ()

  let read_all t =
    Tutils.mutexify t.m (fun () ->
      let ret =
        Buffer.sub t.buf t.offset (Buffer.length t.buf - t.offset);
      in
      t.offset <- Buffer.length t.buf;
      ret) ()

  let advance t =
    Tutils.mutexify t.m (fun () ->
      Utils.buffer_drop t.buf t.offset;
      t.offset <- 0) ()

  let clear t =
    Tutils.mutexify t.m (fun () ->
      Buffer.clear t.buf;
      t.offset <- 0) ()
end

(* {1 External Input handling} *)

class virtual base ~name ~kind ~restart ~restart_on_error
                   ~on_data ?read_header command =
  let reader = Async_read.init () in
  let read = Async_read.read reader in
  let header_read, read_header = match read_header with
    | None   -> true, fun _ -> assert false
    | Some f -> false, f
  in
object (self)
  inherit Source.source ~name kind

  val mutable process = None
  val mutable header_read = header_read

  method stype = Source.Fallible

  method wake_up _ =
    let on_stdout in_chan =
      let s = Process_handler.read 1024 in_chan in
      Async_read.add_bytes reader s;
      try
        let ret =
          if not header_read then
           begin
            read_header read;
            self#log#info "Header read!";
            header_read <- true;
            `Continue
           end
          else on_data reader
        in
        Async_read.advance reader;
        ret
      with Async_read.Not_enough_data ->
        Async_read.reset reader;
        `Continue
    in
    let on_stderr in_chan =
      self#log#info "%s" (Bytes.unsafe_to_string (Process_handler.read 1024 in_chan));
      `Continue
    in
    let on_stop status =
      header_read <- false;
      match status with
        | `Status (Unix.WEXITED 0) -> restart
        | _ -> restart_on_error
    in
    let log = self#log#important "%s" in
    let on_start _ =
      Async_read.clear reader;
      `Continue
    in
    process <- Some (Process_handler.run ~on_stop ~on_stdout ~on_start
                                         ~on_stderr ~log command)

  method sleep =
    match process with
      | Some h ->
          Process_handler.kill h;
          process <- None
      | None -> ()
end
