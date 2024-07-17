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

(** Protocol plugin using ocaml-fetch *)

open Dtools

let dlog n l = Log.logl ~label:"fetch" n (lazy l)

external core_exit : int -> 'a = "unix_exit"

let time = Unix.gettimeofday

let fetch proto arg ~log maxtime =
  if String.length arg > 4 then
    let ext = String.sub arg ((String.length arg)-4) 4 in
    let local = Filename.temp_file "fetch" (String.lowercase ext) in
    let url = proto ^ ":" ^ arg in
    let t0 = time () in
    let ret =
      let pid_dl = flush_all () ; Unix.fork () in
        if pid_dl = 0 then
          begin
            try
              let remaining = int_of_float (maxtime -. time ()) in
                Sys.set_signal Sys.sigalrm
                  (Sys.Signal_handle (fun _ -> core_exit 2)) ;
                if remaining > 0 then begin
                  ignore (Unix.alarm remaining) ;
                  Fetch.cp url ("file://" ^ local) ;
                  core_exit 0
                end else
                  core_exit 2
            with
              | e -> core_exit 1
          end
        else begin
          dlog 4 (Log.f "Downloading %S to %S (pid=%d)"
                    url local pid_dl);
          let p,r = Unix.waitpid [] pid_dl in
            r
        end
    in
    let t1 = time () -. t0 in
      match ret with
        | Unix.WEXITED 0 ->
            dlog 4 (Log.f "Downloaded %S to %S in %f sec." url local t1) ; 
            [Request.indicator ~temporary:true local]
        | Unix.WEXITED 2 ->
            dlog 4 (Log.f
                     "Timeout (%f sec.) while downloading %S to %S"
                     t1 url local) ;
            ( try Unix.unlink local with _ -> () ) ;
            log (Printf.sprintf "Timeout for %S !" url) ;
            []
        | Unix.WEXITED 1 ->
            dlog 4 (Log.f "Downloading of %S to %S failed in %f sec."
                     url local t1) ;
            ( try Unix.unlink local with _ -> () ) ;
            log (Printf.sprintf "Failed to download %S !" url) ;
            []
        | _ ->
            dlog 4 (Log.f "Unknown error while downloading %S to %S (%f sec.)"
                     url local t1) ;
            ( try Unix.unlink local with _ -> () ) ;
            log (Printf.sprintf "Failed to download %S !" url) ;
            []
  else
    ( log ("Invalid URL " ^ proto ^ ":" ^ arg) ; [] )

let _ =
  List.iter
  (fun p -> Request.protocols#register p { Request.resolve = fetch p ;
                                           Request.static  = true } )
    (Fetch.supported_protocols ())
