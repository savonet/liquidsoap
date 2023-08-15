(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2023 Savonet team

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

type clock_variable = Source.clock_variable
type source = Source.source
type active_source = Source.active_source

include Source.Clock_variables

let create_known s = create_known (s :> Source.clock)
let log = Log.make ["clock"]

let conf_clock =
  Dtools.Conf.void ~p:(Configure.conf#plug "clock") "Clock settings"

let conf_clock_preferred =
  Dtools.Conf.string ~d:"posix"
    ~p:(conf_clock#plug "preferred")
    "Preferred clock implementation. One if: \"posix\" or \"ocaml\"."

let time_implementation () =
  try Hashtbl.find Liq_time.implementations conf_clock_preferred#get
  with Not_found -> Liq_time.unix

let () =
  Lifecycle.on_init (fun () ->
      let module Time = (val time_implementation () : Liq_time.T) in
      log#important "Using %s implementation for latency control"
        Time.implementation)

(** [started] indicates that the application has loaded and started
  * its initial configuration; it is set after the first collect.
  * It is mostly intended to allow different behaviors on error:
  *  - for the initial conf, all errors are fatal
  *  - after that (dynamic code execution, interactive mode) some errors
  *    are not fatal anymore. *)
let started : [ `Yes | `No | `Soon ] Atomic.t = Atomic.make `No

(** Indicates whether the application has started to run or not. *)
let running () = Atomic.get started = `Yes

(** We need to keep track of all used clocks, to have them (un)register
  * new sources. We use a weak table to avoid keeping track forever of
  * clocks that are unused and unusable. *)

module H = struct
  type t = Source.clock

  let equal a b = a = b
  let hash a = Oo.id a
end

module Clocks = Weak.Make (H)

let clocks = Clocks.create 10

(** If true, a clock keeps running when an output fails. Other outputs may
  * still be useful. But there may also be some useless inputs left.
  * If no active output remains, the clock will exit without triggering
  * shutdown. We may need some device to allow this (but active and passive
  * clocks will have to be treated separately). *)
let allow_streaming_errors =
  Dtools.Conf.bool
    ~p:(conf_clock#plug "allow_streaming_errors")
    ~d:false "Handling of streaming errors"
    ~comments:
      [
        "Control the behaviour of clocks when an error occurs during streaming.";
        "This has no effect on errors occurring during source initializations.";
        "By default, any error will cause liquidsoap to shutdown. If errors";
        "are allowed, faulty sources are simply removed and clocks keep \
         running.";
        "Allowing errors can result in complex surprising situations;";
        "use at your own risk!";
      ]

let conf_log_delay =
  Dtools.Conf.float
    ~p:(conf_clock#plug "log_delay")
    ~d:1. "How often (in seconds) we should indicate catchup errors."

(** Leave a source, ignoring errors *)

let leave ?failed_to_start (s : active_source) =
  try s#leave ?failed_to_start (s :> source)
  with e ->
    let bt = Printexc.get_backtrace () in
    Utils.log_exception ~log ~bt
      (Printf.sprintf "Error when leaving output %s: %s!" s#id
         (Printexc.to_string e))

(** {1 Clock implementation}
  * One could think of several clocks for isolated parts of a script.
  * One can also think of alsa-clocks, etc. *)

let conf =
  Dtools.Conf.void ~p:(Configure.conf#plug "root") "Streaming clock settings"

let conf_max_latency =
  Dtools.Conf.float ~p:(conf#plug "max_latency") ~d:60.
    "Maximum latency in seconds"
    ~comments:
      [
        "If the latency gets higher than this value, the outputs will be reset,";
        "instead of trying to catch it up second by second.";
        "The reset is typically only useful to reconnect icecast mounts.";
      ]

(** Timing stuff, make sure the frame rate is correct. *)

let sync_descr = function
  | `Auto -> "auto-sync"
  | `CPU -> "CPU sync"
  | `None -> "no sync"

module MkClock (Time : Liq_time.T) = struct
  open Time

  let time_unit = Time.of_float 1.
  let time_zero = Time.of_float 0.

  class clock ?(start = true) ?on_error ?(sync = `Auto) id =
    object (self)
      initializer Clocks.add clocks (self :> Source.clock)
      method id = id
      method sync_mode : Source.sync = sync
      method start = start
      val log = Log.make ["clock"; id]

      (* List of outputs, together with a flag indicating their status:
       *   `New, `Starting, `Aborted, `Active, `Old
       * The list needs to be accessed within critical section of [lock]. *)
      val mutable outputs = []
      val lock = Mutex.create ()
      val mutable can_attach = true

      method stop =
        Tutils.mutexify lock (fun () -> can_attach <- false) ();
        self#detach (fun _ -> true)

      method attach s =
        Tutils.mutexify lock
          (fun () ->
            if can_attach && not (List.exists (fun (_, s') -> s = s') outputs)
            then outputs <- (`New, s) :: outputs)
          ()

      method detach test =
        Tutils.mutexify lock
          (fun () ->
            outputs <-
              List.fold_left
                (fun outputs (flag, s) ->
                  if test s then (
                    match flag with
                      | `New -> outputs
                      | `Active -> (`Old, s) :: outputs
                      | `Starting -> (`Aborted, s) :: outputs
                      | `Old | `Aborted -> (flag, s) :: outputs)
                  else (flag, s) :: outputs)
                [] outputs)
          ()

      method is_attached o = List.exists (fun (_, o') -> o == o') outputs
      val mutable sub_clocks : Source.clock_variable list = []
      method sub_clocks = sub_clocks

      method attach_clock c =
        if not (List.mem c sub_clocks) then sub_clocks <- c :: sub_clocks

      method detach_clock c =
        assert (List.mem c sub_clocks);
        sub_clocks <- List.filter (fun c' -> c <> c') sub_clocks

      val mutable round = 0
      method get_tick = round
      val mutable running = false

      val do_running =
        let lock = Mutex.create () in
        fun f -> Tutils.mutexify lock f ()

      val mutable self_sync = None
      val mutable t0 = time ()
      val mutable ticks = 0L

      method private self_sync =
        let new_val =
          match sync with
            | `Auto ->
                List.exists
                  (fun (state, s) ->
                    state = `Active && snd s#self_sync && s#is_ready)
                  outputs
            | `CPU -> false
            | `None -> true
        in
        begin
          match (self_sync, new_val) with
            | None, false | Some true, false ->
                log#important "Delegating synchronization to CPU clock";
                t0 <- time ();
                ticks <- 0L
            | None, true | Some false, true ->
                log#important "Delegating synchronization to active sources"
            | _ -> ()
        end;
        self_sync <- Some new_val;
        new_val

      method private run =
        let acc = ref 0 in
        let log_delay = Time.of_float conf_log_delay#get in
        let max_latency = Time.of_float (-.conf_max_latency#get) in
        let last_latency_log = ref (time ()) in
        t0 <- time ();
        ticks <- 0L;
        let frame_duration = Time.of_float (Lazy.force Frame.duration) in
        let target_time () =
          t0
          |+| (frame_duration
              |*| Time.of_float (Int64.to_float (Int64.add ticks 1L)))
        in
        log#important "Streaming loop starts in %s mode" (sync_descr sync);
        let rec loop () =
          (* Stop running if there is no output or we're shutting down. *)
          if outputs = [] then ()
          else (
            let self_sync = self#self_sync in
            let target_time = target_time () in
            let rem =
              if self_sync then time_zero else target_time |-| time ()
            in
            (* Sleep a while or worry about the latency *)
            if self_sync || time_zero |<| rem then (
              acc := 0;
              if time_zero |<| rem then sleep_until target_time)
            else (
              incr acc;
              if rem |<| max_latency then (
                log#severe "Too much latency! Resetting active sources...";
                List.iter (function `Active, s -> s#reset | _ -> ()) outputs;
                t0 <- time ();
                ticks <- 0L;
                acc := 0)
              else if
                (rem |<=| (time_zero |-| time_unit) || !acc >= 100)
                && !last_latency_log |+| log_delay |<| time ()
              then (
                last_latency_log := time ();
                log#severe "We must catchup %.2f seconds%s!"
                  (Time.to_float (time_zero |-| rem))
                  (if !acc <= 100 then ""
                   else " (we've been late for 100 rounds)");
                acc := 0));
            ticks <- Int64.add ticks 1L;
            (* This is where the streaming actually happens: *)
            self#end_tick;
            loop ())
        in
        loop ();
        do_running (fun () -> running <- false);
        log#important "Streaming loop stopped."

      val thread_name = "clock_" ^ id
      val mutable on_before_output = []
      method on_before_output fn = on_before_output <- fn :: on_before_output
      val mutable on_output = []
      method on_output fn = on_output <- fn :: on_output
      val mutable on_after_output = []
      method on_after_output fn = on_after_output <- fn :: on_after_output

      (** This is the main streaming step *)
      method end_tick =
        let leaving, active =
          Tutils.mutexify lock
            (fun () ->
              let new_outputs, leaving, active =
                List.fold_left
                  (fun (outputs, leaving, active) (flag, (s : active_source)) ->
                    match flag with
                      | `Old -> (outputs, s :: leaving, active)
                      | `Active -> ((flag, s) :: outputs, leaving, s :: active)
                      | _ -> ((flag, s) :: outputs, leaving, active))
                  ([], [], []) outputs
              in
              outputs <- new_outputs;
              (leaving, active))
            ()
        in
        List.iter (fun (s : active_source) -> leave s) leaving;
        let todo = on_before_output in
        on_before_output <- [];
        List.iter (fun fn -> fn ()) todo;
        let error =
          List.fold_left
            (fun e s ->
              try
                s#output;
                e
              with exn -> (
                let bt = Printexc.get_raw_backtrace () in
                match on_error with
                  | None ->
                      log#severe "Source %s failed while streaming: %s!\n%s"
                        s#id (Printexc.to_string exn)
                        (Printexc.raw_backtrace_to_string bt);
                      leave ~failed_to_start:true s;
                      s :: e
                  | Some on_error ->
                      on_error exn bt;
                      e))
            [] active
        in
        let todo = on_output in
        on_output <- [];
        List.iter (fun fn -> fn ()) todo;
        if error <> [] then (
          Tutils.mutexify lock
            (fun () ->
              outputs <-
                List.filter (fun (_, s) -> not (List.mem s error)) outputs)
            ();

          (* To stop this clock it would be enough to detach all sources
           * and let things stop by themselves. We stop all sources by
           * calling Tutils.shutdown, which calls Clock.stop, stopping
           * all clocks.
           * In any case, we can't just raise an exception here, otherwise
           * the streaming thread (method private run) will die and won't
           * be able to leave all sources. *)
          if not allow_streaming_errors#get then Tutils.shutdown 1);
        round <- round + 1;
        let todo = on_after_output in
        on_after_output <- [];
        List.iter (fun fn -> fn ()) todo

      method start_outputs f =
        let f s = (not (Tutils.finished ())) && f s in
        (* Extract the list of outputs to start, mark them as Starting
         * so they are not managed by a nested call of start_outputs
         * (triggered by collect, which can be triggered by the
         *  starting of outputs).
         *
         * It would be simpler to let the streaming loop (or #end_tick) take
         * care of initialization, just like it takes care of shutting sources
         * down. But this way we guarantee that sources created "simultaneously"
         * start streaming simultaneously. *)
        let to_start =
          Tutils.mutexify lock
            (fun () ->
              let rec aux (outputs, to_start) = function
                | (`New, s) :: tl when f s ->
                    aux ((`Starting, s) :: outputs, s :: to_start) tl
                | (flag, s) :: tl -> aux ((flag, s) :: outputs, to_start) tl
                | [] -> (outputs, to_start)
              in
              let new_outputs, to_start = aux ([], []) outputs in
              outputs <- new_outputs;
              to_start)
            ()
        in
        fun () ->
          let to_start =
            if to_start <> [] then
              log#info "Starting source(s): %s"
                (String.concat ", " (List.map (fun s -> s#id) to_start));
            List.map
              (fun (s : active_source) ->
                try
                  s#get_ready [(s :> source)];
                  `Started s
                with e ->
                  let bt = Printexc.get_backtrace () in
                  log#severe "Source %s failed while starting: %s!\n%s" s#id
                    (Printexc.to_string e) bt;
                  leave ~failed_to_start:true s;
                  `Error s)
              to_start
          in
          (* Now mark the started sources as `Active,
           * unless they have been deactivating in the meantime (`Aborted)
           * in which case they have to be cleanly stopped. *)
          let leaving, errors =
            Tutils.mutexify lock
              (fun () ->
                let new_outputs, leaving, errors =
                  List.fold_left
                    (fun (outputs, leaving, errors) (flag, s) ->
                      if List.mem (`Started s) to_start then (
                        match flag with
                          | `Starting ->
                              ((`Active, s) :: outputs, leaving, errors)
                          | `Aborted -> (outputs, s :: leaving, errors)
                          | `New | `Active | `Old -> assert false)
                      else if List.mem (`Error s) to_start then (
                        match flag with
                          | `Starting -> (outputs, leaving, s :: errors)
                          | `Aborted -> (outputs, leaving, s :: errors)
                          | `New | `Active | `Old -> assert false)
                      else ((flag, s) :: outputs, leaving, errors))
                    ([], [], []) outputs
                in
                outputs <- new_outputs;
                (leaving, errors))
              ()
          in
          if Atomic.get started <> `Yes && errors <> [] then Tutils.shutdown 1;
          if leaving <> [] then (
            log#info "Stopping %d sources..." (List.length leaving);
            List.iter (fun (s : active_source) -> leave s) leaving);
          if
            self#start
            && List.exists (function `Active, _ -> true | _ -> false) outputs
          then
            do_running (fun () ->
                if not running then (
                  running <- true;
                  ignore (Tutils.create (fun () -> self#run) () thread_name)));
          errors
    end
end

let clock ?start ?on_error ?sync id =
  let module Time = (val time_implementation () : Liq_time.T) in
  let module ClockImpl = MkClock (Time) in
  new ClockImpl.clock ?start ?on_error ?sync id

(** {1 Global clock management} *)

(** When created, sources have a clock variable, which gets unified
  * with other variables or concrete clocks. When the time comes to
  * initialize the source, if its clock isn't defined yet, it gets
  * assigned to a default clock and that clock will take care of
  * starting it.
  *
  * Taking all freshly created sources, assigning them to the default
  * clock if needed, and starting them, is performed by [collect].
  * This is typically called after each script execution.
  * Technically we could separate collection and clock assignment,
  * which might simplify some things if it becomes unmanageable in the
  * future.
  *
  * Sometimes we need to be sure that collect doesn't happen during
  * the execution of a function. Otherwise, sources might be assigned
  * the default clock too early. This is done using [collect_after].
  * This need is not cause by running collect in too many places, but
  * simply because there is no way to control collection on a per-thread
  * basis (collect only the sources created by a given thread of
  * script execution).
  *
  * Functions running using [collect_after] should be kept short.
  * However, in theory, with multiple threads, we could have plenty
  * of short functions always overlapping so that collection can
  * never be done. This shouldn't happen too much, but in any case
  * we can't get rid of this without a more fine-grained collect,
  * which would require (heavy) execution contexts to tell from
  * which thread/code a given source has been added. *)

(** We must keep track of the number of tasks currently executing
  * in a collect_after. When the last one exits it must collect.
  *
  * It is okay to start a new collect_after when a collect is
  * ongoing: all that we're doing is avoiding collection of sources
  * created by the task. That's why #start_outputs first harvests
  * sources then returns a function actually starting those sources:
  * only the first part is done within critical section.
  *
  * The last trick is that we start with a fake task (after_collect_tasks=1)
  * to make sure that the initial parsing of files does not triggers collect and thus
  * a too early initialization of outputs (before daemonization). Main is
  * in charge of finishing that virtual task and trigger the initial
  * collect. *)
let after_collect_tasks = ref 1

let lock = Mutex.create ()

(** We might not need a default clock, so we use a lazy clock value.
  * We don't use Lazy because we need a thread-safe mechanism. *)
let get_default = Tutils.lazy_cell (fun () -> (clock "main" :> Source.clock))

let create_follow_clock id = (clock ~start:false id :> Source.clock)

(** A function displaying the varying number of allocated clocks. *)
let gc_alarm =
  let last_displayed = ref (-1) in
  fun () ->
    let nb_clocks = Clocks.count clocks in
    if nb_clocks <> !last_displayed then (
      log#info "Currently %d clock(s) allocated." nb_clocks;
      last_displayed := nb_clocks)

let () = ignore (Gc.create_alarm gc_alarm)

(** After some sources have been created or removed (by script execution),
  * finish assigning clocks to sources (assigning the default clock),
  * start clocks and sources that need starting,
  * and stop those that need stopping. *)
let collect ~must_lock =
  if must_lock then Mutex.lock lock;

  (* If at least one task is engaged it will take care of collection later.
   * Otherwise, prepare a collection while in critical section
   * (to avoid harvesting sources created by a task) and run it
   * outside of critical section (to avoid all sorts of shit). *)
  if !after_collect_tasks > 0 then Mutex.unlock lock
  else (
    Source.iterate_new_outputs (fun o ->
        if not (is_known o#clock) then (
          let clock =
            if should_start o#clock then get_default ()
            else create_follow_clock o#id
          in
          ignore (unify ~pos:o#pos o#clock (create_known clock))));
    gc_alarm ();
    let filter _ = true in
    let collects =
      Clocks.fold (fun s l -> s#start_outputs filter :: l) clocks []
    in
    let start =
      if Atomic.get started <> `No then ignore
      else (
        (* Avoid that some other collection takes up the task
         * to set started := true. Typically they would be
         * trivial (empty) collections terminating before us,
         * which defeats the purpose of the flag. *)
        Atomic.set started `Soon;
        fun () ->
          log#info "Main phase starts.";
          Atomic.set started `Yes)
    in
    Mutex.unlock lock;
    List.iter (fun f -> ignore (f ())) collects;
    start ())

let collect_after f =
  Mutex.lock lock;
  after_collect_tasks := !after_collect_tasks + 1;
  Mutex.unlock lock;
  Fun.protect f ~finally:(fun () ->
      Mutex.lock lock;
      after_collect_tasks := !after_collect_tasks - 1;
      collect ~must_lock:false)

(** Initialize only some sources, recognized by a filter function.
  * The advantage over collect is that it is synchronous and a list
  * of errors (sources that failed to initialize) is returned. *)
let force_init filter =
  let collects =
    Tutils.mutexify lock
      (fun () ->
        Source.iterate_new_outputs (fun o ->
            if filter o && not (is_known o#clock) then
              ignore (unify ~pos:o#pos o#clock (create_known (get_default ()))));
        gc_alarm ();
        Clocks.fold (fun s l -> s#start_outputs filter :: l) clocks [])
      ()
  in
  List.concat (List.map (fun f -> f ()) collects)

let start () =
  Mutex.lock lock;
  after_collect_tasks := !after_collect_tasks - 1;
  collect ~must_lock:false

let stop () = Clocks.iter (fun s -> s#stop) clocks
let fold f x = Clocks.fold f clocks x
