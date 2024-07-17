(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2006 Savonet team

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 *****************************************************************************)

open Types
open Dtools

let dlog = Log.log ~label:"extproto"

let resolve proto program command s ~log maxtime =
  let s = proto ^ ":" ^ s in
  let local = Filename.temp_file "liq" ".osb" in
  (* We create a fresh stdin for the process,
   * and another one, unused by the child, on which we'll wait for EOF
   * as a mean to detect termination. *)
  let (iR,iW) = Unix.pipe () in
  let (xR,xW) = Unix.pipe () in
  let pid =
    Unix.create_process
      program (command s local)
      iR Unix.stderr Unix.stderr
  in
  dlog 4 (Log.f "Executing %s %S %S" program s local) ;
  let timeout = max 0. (maxtime -. Unix.gettimeofday ()) in
    Unix.close iR ;
    Unix.close xW ;
    if Unix.select [xR] [] [] timeout = ([],[],[]) then
      Unix.kill pid 9 ;
    let (p,code) = Unix.waitpid [] pid in
      assert (p <> 0) ;
      dlog 4 (Log.f "Download process finished (%s)"
                (match code with
                   | Unix.WSIGNALED _ -> "killed"
                   | Unix.WEXITED 0 -> "ok"
                   | _ -> "error")) ;
      Unix.close iW ;
      Unix.close xR ;
      if code = Unix.WEXITED 0 then
        [Request.indicator ~temporary:true local]
      else begin
        log "Download failed: timeout, invalid URI ?" ;
        ( try Unix.unlink local with _ -> () ) ;
        []
      end

let _ =
  List.iter
    (fun (prog,protos,command) ->
      List.iter
        (fun proto ->
           Request.protocols#register
             ~sdoc:(Printf.sprintf "Fetch files using %s." prog)
             proto
             { Request.resolve = resolve proto prog command ;
               Request.static = false })
        protos)
    Configure.extproto
