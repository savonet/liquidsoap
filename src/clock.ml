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

(** Base clock class *)

class clock id =
object (self)

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
     * TODO this is actually slightly wrong since collection might arise
     * from another Lang execution in another thread... *)
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
      if to_start <> [] then
        log#f 4 "Starting %d sources..." (List.length to_start) ;
      iter
        (fun (s:active_source) -> s#get_ready [(s:>source)])
        ~rollback:(fun (s:active_source) -> s#leave (s:>source))
        to_start ;
      List.iter
        (fun s -> s#output_get_ready)
        to_start ;
      let leaving =
        Tutils.mutexify lock
          (fun () ->
             let new_outputs, leaving =
               List.fold_left
                 (fun (outputs,leaving) (flag,s) ->
                    if List.mem s to_start then
                      match flag with
                         | `Starting -> (`Active,s)::outputs, leaving
                         | `Aborted -> outputs, s::leaving
                         | `New | `Active | `Old -> assert false
                    else
                      (flag,s)::outputs, leaving)
                 ([],[]) outputs
             in
               outputs <- new_outputs ;
               leaving) ()
      in
        List.iter (fun (s:active_source) -> s#leave (s:>source)) leaving

  val mutable started = false
  method is_started = started

  method start =
    (* Set the flag first to avoid "recursive" calls to #start via collect *)
    started <- true ;
    begin match outputs with
      | [] -> log#f 4 "No active source in this time flow."
      | [_,s] -> log#f 4 "Waking up one active source: %s." s#id
      | _ ->
          log#f 4 "Waking up %d active sources: %s."
            (List.length outputs)
            (String.concat ", "
               (List.map (fun (_,s) -> s#id) outputs))
    end ;
    self#start_outputs

  method stop =
    List.iter
      (function
         | (`Active,(s:active_source)) -> s#leave (s:>source)
         | _ -> ())
      outputs ;
    started <- false

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
    (* if started && not (Tutils.running thread_name thread) then super#stop ;
     * *)
    (* We might be able to omit joining, since this code is currently
     * only ever called by Main which joins everything at once afterwards. *)
    Thread.join thread

end

let default = (new wallclock "main" :> Source.clock)

(** After some sources have been created or removed (by script execution),
  * finish assigning clocks to sources (assigning the default clock),
  * start clocks and sources that need starting,
  * and stop those that need stopping. *)
let collect () =
  let clocks = get_clocks ~default in
  let new_clocks = List.filter (fun c -> not c#is_started) clocks in
    begin match new_clocks with
      | [] -> ()
      | [c] ->
          log#f 4 "Starting one clock: %s..." c#id
      | _ ->
          log#f 4 "Starting %d clocks: %s..."
            (List.length new_clocks)
            (String.concat ", " (List.map (fun c -> c#id) new_clocks))
    end ;
    iter (fun s -> s#start) ~rollback:(fun s -> s#stop) new_clocks ;
    List.iter (fun s -> s#start_outputs) clocks ;
    started := true

let stop () =
  if running () then begin
    (* Gently ask for shutdown. *)
    shutdown := true ;
    List.iter (fun s -> s#stop) (get_clocks ~default)
  end
