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

  let add_string {buf;m} =
    Tutils.mutexify m (Buffer.add_string buf)

  let read t =
    Tutils.mutexify t.m (fun n ->
      if Buffer.length t.buf < n+t.offset then
       begin
        t.offset <- 0;
        raise Not_enough_data
       end;
      let ret = Buffer.sub t.buf t.offset n in
      t.offset <- t.offset + n;
      ret)

  let close t =
    Tutils.mutexify t.m (fun () ->
      Utils.buffer_drop t.buf t.offset;
      Buffer.contents t.buf) ()
end

(* {1 External Input handling} *)

class virtual base ~name ~kind ~restart ~restart_on_error
                   ~on_data ?read_header command =
  let reader = Async_read.init () in
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
      let s = Bytes.unsafe_to_string s in
      if not header_read then
       try
         Async_read.add_string reader s;
         read_header (Async_read.read reader);
         let ret =
           Async_read.close reader
         in
         self#log#info "Header read!";
         header_read <- true;
         if ret <> "" then on_data ret else `Continue
       with Async_read.Not_enough_data -> `Continue
     else on_data s
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
    process <- Some (Process_handler.run ~on_stop ~on_stdout 
                                         ~on_stderr ~log command)

  method sleep =
    match process with
      | Some h ->
          Process_handler.kill h;
          process <- None
      | None -> ()
end
