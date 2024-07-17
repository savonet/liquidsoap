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

open Dtools
open Types

let log = Log.log ~label:"root"

let uptime =
  let base = Unix.time () in
    fun () ->
      (Unix.time ()) -. base

let shutdown = ref false

(** Timing stuff, make sure the frame rate is correct. *)

let time = Unix.gettimeofday
let usleep d = ignore (Unix.select [] [] [] d)
let t0 = ref 0.
let ticks = ref 0L
let acc = ref 0
let d = Mixer.Buffer.length
let delay () = !t0 +. d *. (Int64.to_float (Int64.add !ticks 1L)) -. time ()

let nosync = try ignore (Sys.getenv "NOSYNC") ; true with _ -> false

(** Main loop. *)

let iter = Types.iter_outputs

let started = ref false
let force_sleep () = if !started then iter (fun s -> s#leave s)

let start () =

  log 3 "Waking up active nodes..." ;
  iter (fun s -> s#get_ready [s]) ;
  iter (fun s -> s#output_get_ready) ;
  started := true ;

  log 3 "Broadcast starts up!" ;
  t0 := time () ;
  while not !shutdown do
    let rem = if nosync then 0. else delay () in
      (* Sleep a while or worry about the latency *)
      if nosync || rem > 0. then begin
        acc := 0 ;
        usleep rem
      end else begin
        incr acc ;
        if rem <= -1. || !acc >= 100 then begin
          log 2 (Log.f "We must catchup %.2f sec seconds%s!"
                   (-. rem)
                   (if !acc <= 100 then "" else
                      " (we've been late for 100 rounds)")) ;
          acc := 0
        end
      end ;
      ticks := Int64.add !ticks 1L ;
      iter (fun s -> s#output) ;
      iter (fun s -> s#after_output)
  done ;

  log 2 "Root shutdown" ;
  iter (fun s -> s#leave s)
