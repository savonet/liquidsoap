(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2010 Savonet team

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

open Source

type clock_variable = Source.clock_variable

include Clock_variables

let create_known s = create_known (s:>Source.clock)

let log = Dtools.Log.make ["clock"]

(** Two flags controlling the running of clocks:
  * [started] indicates that clocks are running (or have crashed, but
  * did not shutdown properly);
  * [shutdown] requires that all properly running clocks stop gracefully. *)

let started = ref false
let shutdown = ref false
(* Note that shutdown could be made local to each clock (nicer) except
 * for a few sources that use it to stop too
 * (they should use the local wake_up/sleep mechanism instead). *)

let running () = !started

let iter ~rollback f l =
  let rec aux ran = function
    | [] -> ()
    | c::tl ->
        (try f c with e -> List.iter rollback ran  ; raise e) ;
        aux (c::ran) tl
  in
    aux [] l

(** Base clock class *)

class clock id =
object

  method id = id

  val log = Dtools.Log.make ["clock";id]

  val mutable outputs = []
  method attach s = outputs <- s::outputs

  val mutable sub_clocks : Source.clock_variable list = []
  method attach_clock c = sub_clocks <- c::sub_clocks
  method sub_clocks = sub_clocks

  method end_tick =
    List.iter (fun s -> s#output) outputs ;
    List.iter (fun s -> s#after_output) outputs

  method start =
    begin match outputs with
      | [] -> log#f 4 "No active source in this time flow."
      | [s] -> log#f 4 "Waking up one active source: %s." s#id
      | _ ->
          log#f 4 "Waking up %d active sources: %s."
            (List.length outputs)
            (String.concat ", "
               (List.map (fun s -> s#id) outputs))
    end ;
    iter
      (fun (s:active_source) -> s#get_ready [(s:>source)])
      ~rollback:(fun (s:active_source) -> s#leave (s:>source))
      outputs ;
    List.iter
      (fun s -> s#output_get_ready)
      outputs

  method stop =
    List.iter (fun (s:active_source) -> s#leave (s:>source)) outputs

end

(** {1 Wallclock implementation}
  * This was formerly known as the Root.
  * One could think of several wallclocks for isolated parts of a script.
  * One can also think of alsa-clocks, etc. *)

open Dtools

let conf =
  Conf.void ~p:(Configure.conf#plug "root") "Streaming clock settings"
let conf_max_latency =
  Conf.float ~p:(conf#plug "max_latency") ~d:60. "Maximum latency in seconds"
    ~comments:[
      "If the latency gets higher than this value, the outputs will be reset,";
      "instead of trying to catch it up second by second." ;
      "The reset is typically only useful to reconnect icecast mounts."
    ]
let conf_sync =
  Conf.bool ~p:(conf#plug "sync") ~d:true "Synchronization flag"
    ~comments:[
      "Control whether or not liquidsoap should take care of the timing.";
      "Otherwise, the sources may handle it by themselves -- typically in the ";
      "case of un-bufferized alsa I/O, which turns root synchronization off";
      "automatically.";
      "Leaving the sources without synchronization can also be useful for ";
      "debugging or measuring performance, as it results in liquidsoap running";
      "as fast as possible."
    ]

(** Timing stuff, make sure the frame rate is correct. *)

let time = Unix.gettimeofday
let usleep d =
  (* In some implementations,
   * Thread.delay uses Unix.select which can raise EINTR.
   * A really good implementation would keep track of the elapsed time and then
   * trigger another Thread.delay for the remaining time.
   * This cheap thing does the job for now.. *)
  try Thread.delay d with Unix.Unix_error (Unix.EINTR,_,_) -> ()

class wallclock ?sync id =
object (self)

  inherit clock ("wallclock_"^id) as super

  (** Main loop. *)

  method private run =
    let acc = ref 0 in
    let max_latency = -. conf_max_latency#get in
    let sync = match sync with None -> conf_sync#get | Some b -> b in
    let last_latency_log = ref (time ()) in
    let t0 = ref (time ()) in
    let ticks = ref 0L in
    let delay () =
      !t0
      +. (Lazy.force Frame.duration) *. Int64.to_float (Int64.add !ticks 1L)
      -. time ()
    in
      if sync then
        log#f 3 "Streaming loop starts, real time rate (wallclock mode)."
      else
        log#f 3 "Streaming loop starts, maximum time rate (CPU-burn mode)." ;
      while not !shutdown do
        let rem = if not sync then 0. else delay () in
          (* Sleep a while or worry about the latency *)
          if (not sync) || rem > 0. then begin
            acc := 0 ;
            usleep rem
          end else begin
            incr acc ;
            if rem < max_latency then begin
              log#f 2 "Too much latency! Resetting active sources.." ;
              List.iter (fun s -> if s#is_active then s#output_reset) outputs ;
              t0 := time () ;
              ticks := 0L ;
              acc := 0
            end else if
              (rem <= -1. || !acc >= 100) && !last_latency_log +. 1. < time ()
            then begin
              last_latency_log := time () ;
              log#f 2 "We must catchup %.2f seconds%s!"
                (-. rem)
                (if !acc <= 100 then "" else
                   " (we've been late for 100 rounds)") ;
              acc := 0
            end
          end ;
          ticks := Int64.add !ticks 1L ;
          (* This is where the streaming actually happens: *)
          super#end_tick
      done ;
      (* Clocks have been asked to stop. *)
      super#stop

  val mutable thread = Thread.self ()

  val thread_name = "wallclock_" ^ id

  method start =
    (* Wake up outputs. *)
    super#start ;
    (* Have them work in rhythm. *)
    thread <- Tutils.create (fun () -> self#run) () thread_name

  method stop =
    (* If #run has crashed, do the cleanup ourselves. *)
    if not (Tutils.running thread_name thread) then super#stop ;
    (* We might be able to omit joining, since this code is currently
     * only ever called by Main which joins everything at once afterwards. *)
    Thread.join thread

end

let default = (new wallclock "main" :> Source.clock)

let start () =
  let clocks = get_clocks ~default in
    begin match clocks with
      | [] -> assert false
      | [c] -> log#f 4 "There is only one clock: %s." c#id
      | _ ->
          log#f 4 "There are %d clocks: %s."
            (List.length clocks)
            (String.concat ", " (List.map (fun c -> c#id) clocks))
    end ;
    iter
      (fun s -> s#start) ~rollback:(fun s -> s#stop)
      clocks ;
    started := true

let stop () =
  if running () then begin
    (* Gently ask for shutdown. *)
    shutdown := true ;
    List.iter (fun s -> s#stop) (get_clocks ~default)
  end
