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

type clock_variable = Source.clock_variable
type source = Source.source
type active_source = Source.active_source

include Source.Clock_variables

let create_known s = create_known (s:>Source.clock)

let log = Dtools.Log.make ["clock"]

(** [started] indicates that the application has loaded and started
  * its initial configuration.
  * It is mostly intended to allow different behaviors on error:
  *  - for the initial conf, all errors are fatal
  *  - after that (dynamic code execution, interactive mode) some errors
  *    are not fatal anymore. *)

let started = ref false
let running () = !started
let set_running () = started := true

(** If initialization raises an exception, we want to report it and shutdown.
  * However, this has to be done carefully, by un-initializing first:
  * otherwise shutdown will hang (and temporary files may remain, etc). *)
let iter ~rollback f l =
  let rec aux ran = function
    | [] -> ()
    | c::tl ->
        (try f c with e -> List.iter rollback ran  ; raise e) ;
        aux (c::ran) tl
  in
    aux [] l

(** We need to keep track of all used clocks, to have them (un)register
  * new sources. We use a weak table to avoid keeping track forever of
  * clocks that are unused and unusable. *)

module H = struct
  type t = Source.clock
  let equal a b = a = b
  let hash a = Oo.id a
end
module Clocks = Weak.Make(H)
let clocks = Clocks.create 10

(** Base clock class *)

class clock id =
object (self)

  initializer Clocks.add clocks (self:>Source.clock)

  method id = id

  val log = Dtools.Log.make ["clock";id]

  val lock = Mutex.create ()
  val mutable outputs = []

  method attach s =
    Tutils.mutexify lock
      (fun () ->
         if not (List.exists (fun (_,s') -> s=s') outputs) then
           outputs <- (`New,s)::outputs)
      ()

  method detach test =
    Tutils.mutexify lock
      (fun () ->
         outputs <-
           List.fold_left
             (fun outputs (flag,s) ->
                if test s then
                  match flag with
                    | `New -> outputs
                    | `Active -> (`Old,s)::outputs
                    | `Starting -> (`Aborted,s)::outputs
                    | `Old | `Aborted -> (flag,s)::outputs
                else
                  (flag,s)::outputs)
             [] outputs) ()

  val mutable sub_clocks : Source.clock_variable list = []
  method sub_clocks = sub_clocks
  method attach_clock c =
    if not (List.mem c sub_clocks) then sub_clocks <- c::sub_clocks

  val mutable round = 0

  method get_tick = round

  method end_tick =
    let leaving,outputs =
      Tutils.mutexify lock
        (fun () ->
           let new_outputs,leaving,active =
             List.fold_left
               (fun (outputs,leaving,active) (flag,(s:active_source)) ->
                  match flag with
                    | `Old -> outputs, s::leaving, active
                    | `Active -> (flag,s)::outputs, leaving, s::active
                    | _ -> (flag,s)::outputs, leaving, active)
               ([],[],[])
               outputs
           in
             outputs <- new_outputs ;
             leaving,active) ()
    in
      List.iter (fun (s:active_source) -> s#leave (s:>source)) leaving ;
      List.iter (fun s -> s#output) outputs ;
      round <- round + 1 ;
      List.iter (fun s -> s#after_output) outputs

  method start_outputs =
    (* Extract the list of outputs to start, mark them as Starting
     * so they are not managed by a nested call of start_outputs
     * (triggered by collect, which can be triggered by the
     *  starting of outputs).
     *
     * It would be simpler to let the streaming loop (or #end_tick) take
     * care of initialization, just like it takes care of shutting sources
     * down. But this way we guarantee that sources created "simultaneously"
     * start streaming simultaneously.
     *
     * The simultaneity is actually not quite there, since collections
     * can be run from several threads running Lang code (and a collection
     * from one thread/code will start outputs from the other). To avoid
     * this we would need execution contexts, but I'm happy to not implement
     * that for now. *)
    let to_start =
      Tutils.mutexify lock
        (fun () ->
           let rec aux (outputs,to_start) = function
             | (`New,s)::tl -> aux ((`Starting,s)::outputs,s::to_start) tl
             | (flag,s)::tl -> aux ((flag,s)::outputs,to_start) tl
             | [] -> outputs,to_start
           in
           let new_outputs,to_start = aux ([],[]) outputs in
             outputs <- new_outputs ;
             to_start)
        ()
    in
    let to_start =
      if to_start <> [] then
        log#f 4 "Starting %d sources..." (List.length to_start) ;
      List.map
        (fun (s:active_source) ->
           try s#get_ready [(s:>source)] ; `Woken_up s with
             | e when !started ->
                 log#f 2 "Error when starting %s: %s!"
                   s#id (Printexc.to_string e) ;
                 `Error s)
        to_start
    in
    let to_start =
      List.map
        (function
           | `Error s -> `Error s
           | `Woken_up (s:active_source) ->
               try s#output_get_ready ; `Started s with
                 | e when !started ->
                     log#f 2 "Error when starting %s: %s!"
                       s#id (Printexc.to_string e) ;
                     s#leave (s:>source) ;
                     `Error s)
        to_start
    in
    (* Now mark the started sources as `Active,
     * unless they have been deactivating in the meantime (`Aborted)
     * in which case they have to be cleanly stopped. *)
    let leaving =
      Tutils.mutexify lock
        (fun () ->
           let new_outputs, leaving =
             List.fold_left
               (fun (outputs,leaving) (flag,s) ->
                  if List.mem (`Started s) to_start then
                    match flag with
                       | `Starting -> (`Active,s)::outputs, leaving
                       | `Aborted -> outputs, s::leaving
                       | `New | `Active | `Old -> assert false
                  else if List.mem (`Error s) to_start then
                    match flag with
                       | `Starting -> outputs, leaving
                       | `Aborted -> outputs, leaving
                       | `New | `Active | `Old -> assert false
                  else
                    (flag,s)::outputs, leaving)
               ([],[]) outputs
           in
             outputs <- new_outputs ;
             leaving) ()
    in
      if leaving <> [] then
        log#f 4 "Stopping %d sources..." (List.length leaving) ;
      List.iter (fun (s:active_source) -> s#leave (s:>source)) leaving

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

  val mutable running = false
  val do_running =
    let lock = Mutex.create () in
      fun f -> Tutils.mutexify lock f ()

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
      let rec loop () =
        if outputs = [] then () else
        let rem = if not sync then 0. else delay () in
          (* Sleep a while or worry about the latency *)
          if (not sync) || rem > 0. then begin
            acc := 0 ;
            usleep rem
          end else begin
            incr acc ;
            if rem < max_latency then begin
              log#f 2 "Too much latency! Resetting active sources.." ;
              List.iter
                (function
                   | (`Active,s) when s#is_active -> s#output_reset
                   | _ -> ())
                outputs ;
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
          super#end_tick ;
          loop ()
      in
        loop () ;
        do_running (fun () -> running <- false) ;
        log#f 3 "Streaming loop stopped."

  val thread_name = "wallclock_" ^ id

  method start_outputs =
    super#start_outputs ;
    if List.exists (function (`Active,_) -> true | _ -> false) outputs then
      do_running
        (fun () ->
           (* TODO this might be too early: this could be a nested #start_o
            *   in which case we'd prefer the outer one to start the thread *)
           if not running then begin
             running <- true ;
             ignore (Tutils.create (fun () -> self#run) () thread_name)
           end)

end

let default = (new wallclock "main" :> Source.clock)

(** After some sources have been created or removed (by script execution),
  * finish assigning clocks to sources (assigning the default clock),
  * start clocks and sources that need starting,
  * and stop those that need stopping.
  *
  * TODO the next step is to get rid of (non-weak) references to clocks
  *   so that they can be garbage collected when they stop running
  *   the purpose of the list of clock variables is only to assign the
  *   default clock currently, which we could do simply by keeping a list
  *   of freshly created outputs
  *
  *   we do need a list of ALL clocks, because source.deactivate() could
  *   require a collect on an old clock... unless we maintain a list
  *   of clocks that may require collection *)
let collect () =
  assign_clocks ~default ;
  log#f 4 "Currently %d clocks allocated." (Clocks.count clocks) ;
  Clocks.iter (fun s -> s#start_outputs) clocks

(** To stop, simply detach everything and the clocks will stop running. *)
let stop () =
  Clocks.iter (fun s -> s#detach (fun _ -> true)) clocks
