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

(* {1 External Input handling} *)

class virtual base ~name ~kind ~restart ~restart_on_error ~on_data ?read_header
  command =
  let header_read, read_header =
    match read_header with
      | None -> (true, fun _ -> assert false)
      | Some f -> (false, f)
  in
  object (self)
    inherit Source.source ~name kind

    val mutable process = None

    val mutable header_read = header_read

    method stype = Source.Fallible

    method wake_up _ =
      let on_stdout reader =
        if not header_read then (
          let ret = read_header reader in
          self#log#info "Header read!";
          header_read <- true;
          ret )
        else on_data reader
      in
      let on_stderr =
        let buf = Bytes.create Utils.pagesize in
        fun puller ->
          let len = puller buf 0 Utils.pagesize in
          self#log#info "%s" (Bytes.unsafe_to_string (Bytes.sub buf 0 len));
          `Continue
      in
      let on_stop status =
        header_read <- false;
        match status with
          | `Status (Unix.WEXITED 0) -> restart
          | _ -> restart_on_error
      in
      let log = self#log#important "%s" in
      process <-
        Some
          (Process_handler.run ~priority:Tutils.Blocking ~on_stop ~on_stdout
             ~on_stderr ~log command)

    method sleep =
      match process with
        | Some h ->
            Process_handler.kill h;
            process <- None
        | None -> ()
  end
