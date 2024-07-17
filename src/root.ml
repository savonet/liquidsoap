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
open Source

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
let d = Mixer.Buffer.length
let delay () = !t0 +. d *. (Int64.to_float (Int64.add !ticks 1L)) -. time ()

let nosync = try ignore (Sys.getenv "NOSYNC") ; true with _ -> false

(** Main loop. *)

let iter = Source.iter_outputs

let started = ref false
let force_sleep () = if !started then iter (fun s -> s#leave (s:>Source.source))

let _ =
  Dtools.Var.register "max_latency" Dtools.Var.Float

let start () =

  let acc = ref 0 in
  let max_latency = Conf.get_float ~default:60. "max_latency" in
  let max_latency = -. max_latency in
  let last_latency_log = ref (time ()) in

  log 3 "Waking up active nodes..." ;
  iter (fun s -> s#get_ready [(s:>Source.source)]) ;
  iter (fun s -> s#output_get_ready) ;
  if Source.has_outputs () then
    started := true
  else begin
    log 2
      ("The script didn't define any output or active source! "^
       "Nothing to do: exiting...") ;
    shutdown := true
  end ;

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
        if rem < max_latency then begin
          log 2 "Too much latency! Resetting active sources.." ;
          iter (fun s -> s#output_reset) ;
          t0 := time () ;
          ticks := 0L ;
          acc := 0
        end else if
          (rem <= -1. || !acc >= 100) && !last_latency_log +. 1. < time ()
        then begin
          last_latency_log := time () ;
          log 2 (Log.f "We must catchup %.2f seconds%s!"
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
  iter (fun s -> s#leave (s:>Source.source)) ;
  (* In case of a "clean" root shutdown (e.g. when there's no output)
   * we have to raise an exception to trigger the global shutdown. *)
  if not !started then failwith "This is not a crash."
