(*****************************************************************************

  Liquidsoap, a programmable stream generator.
  Copyright 2003-2026 Savonet team

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

let runtime = Modules.runtime
let runtime_gc = Lang.add_module ~base:runtime "gc"

(* Which domain the calling code runs on. The major heap is per-domain, so a
   task that wants every domain collected has to notice which one it landed on
   and repeat until it has seen them all. *)
let _ =
  Lang.add_builtin ~base:runtime "domain" ~category:`Liquidsoap
    ~descr:"Index of the domain the calling code is running on." [] Lang.int_t
    (fun _ -> Lang.int (Domain.self () :> int))

let _ =
  Lang.add_builtin ~base:runtime "domain_count" ~category:`Liquidsoap
    ~descr:"Number of domains the runtime recommends for this machine." []
    Lang.int_t (fun _ -> Lang.int (Domain.recommended_domain_count ()))

let _ =
  Lang.add_builtin ~base:runtime_gc "full_major" ~category:`Liquidsoap
    ~descr:"Trigger full major garbage collection." [] Lang.unit_t (fun _ ->
      Gc.full_major ();
      Lang.unit)

let _ =
  Lang.add_builtin ~base:runtime_gc "minor" ~category:`Liquidsoap
    ~descr:"Trigger full minor garbage collection." [] Lang.unit_t (fun _ ->
      Gc.minor ();
      Lang.unit)

let _ =
  Lang.add_builtin ~base:runtime_gc "major_slice" ~category:`Liquidsoap
    ~descr:
      "Do a minor collection and a slice of major collection. The optional \
       argument `n` is the size of the slice: the GC will do enough work to \
       free (on average) `n` words of memory. If `0` (its default), the GC \
       will try to do enough work to ensure that the next automatic slice has \
       no work to do."
    [("", Lang.int_t, Some (Lang.int 0), Some "Size of the slice")]
    Lang.unit_t
    (fun p ->
      ignore (Gc.major_slice (Lang.to_int (List.assoc "" p)));
      Lang.unit)

let _ =
  Lang.add_builtin ~base:runtime_gc "major" ~category:`Liquidsoap
    ~descr:
      "Trigger a minor collection and finish the current major collection \
       cycle.." [] Lang.unit_t (fun _ ->
      Gc.major ();
      Lang.unit)

let _ =
  Lang.add_builtin ~base:runtime_gc "compact" ~category:`Liquidsoap
    ~descr:
      "Perform a full major collection and compact the heap. Note that heap \
       compaction is a lengthy operation." [] Lang.unit_t (fun _ ->
      Gc.compact ();
      Lang.unit)

let _ =
  let stat_t =
    Lang.record_t
      [
        ("minor_words", Lang.float_t);
        ("promoted_words", Lang.float_t);
        ("major_words", Lang.float_t);
        ("minor_collections", Lang.int_t);
        ("major_collections", Lang.int_t);
        ("forced_major_collections", Lang.int_t);
        ("heap_words", Lang.int_t);
        ("heap_chunks", Lang.int_t);
        ("live_words", Lang.int_t);
        ("live_blocks", Lang.int_t);
        ("free_words", Lang.int_t);
        ("free_blocks", Lang.int_t);
        ("largest_free", Lang.int_t);
        ("fragments", Lang.int_t);
        ("compactions", Lang.int_t);
        ("top_heap_words", Lang.int_t);
        ("stack_size", Lang.int_t);
      ]
  in
  let stat
      {
        Gc.minor_words;
        promoted_words;
        major_words;
        minor_collections;
        major_collections;
        forced_major_collections;
        heap_words;
        heap_chunks;
        live_words;
        live_blocks;
        free_words;
        free_blocks;
        largest_free;
        fragments;
        compactions;
        top_heap_words;
        stack_size;
      } =
    Lang.record
      [
        ("minor_words", Lang.float minor_words);
        ("promoted_words", Lang.float promoted_words);
        ("major_words", Lang.float major_words);
        ("minor_collections", Lang.int minor_collections);
        ("major_collections", Lang.int major_collections);
        ("forced_major_collections", Lang.int forced_major_collections);
        ("heap_words", Lang.int heap_words);
        ("heap_chunks", Lang.int heap_chunks);
        ("live_words", Lang.int live_words);
        ("live_blocks", Lang.int live_blocks);
        ("free_words", Lang.int free_words);
        ("free_blocks", Lang.int free_blocks);
        ("largest_free", Lang.int largest_free);
        ("fragments", Lang.int fragments);
        ("compactions", Lang.int compactions);
        ("top_heap_words", Lang.int top_heap_words);
        ("stack_size", Lang.int stack_size);
      ]
  in
  ignore
    (Lang.add_builtin ~base:runtime_gc "stat" ~category:`System
       ~descr:
         "Return the current values of the memory management counters. This \
          function examines every heap block to get the statistics." [] stat_t
       (fun _ -> stat (Gc.stat ())));

  Lang.add_builtin ~base:runtime_gc "quick_stat" ~category:`System
    ~descr:
      "Same as stat except that `live_words`, `live_blocks`, `free_words`, \
       `free_blocks`, `largest_free`, and `fragments` are set to `0`. This \
       function is much faster than `gc.stat` because it does not need to go \
       through the heap." [] stat_t (fun _ -> stat (Gc.quick_stat ()))

let _ =
  Lang.add_builtin ~base:runtime_gc "print_stat" ~category:`System
    ~descr:
      "Print the current values of the memory management counters in \
       human-readable form." [] Lang.unit_t (fun _ ->
      Gc.print_stat stdout;
      flush stdout;
      Lang.unit)

let _ =
  let control_t =
    Lang.record_t
      [
        ("minor_heap_size", Lang.int_t);
        ("major_heap_increment", Lang.int_t);
        ("space_overhead", Lang.int_t);
        ("verbose", Lang.int_t);
        ("max_overhead", Lang.int_t);
        ("stack_limit", Lang.int_t);
        ("allocation_policy", Lang.int_t);
        ("window_size", Lang.int_t);
        ("custom_major_ratio", Lang.int_t);
        ("custom_minor_ratio", Lang.int_t);
        ("custom_minor_max_size", Lang.int_t);
      ]
  in
  let control
      {
        Gc.minor_heap_size;
        major_heap_increment;
        space_overhead;
        verbose;
        max_overhead;
        stack_limit;
        allocation_policy;
        window_size;
        custom_major_ratio;
        custom_minor_ratio;
        custom_minor_max_size;
      } =
    Lang.record
      [
        ("minor_heap_size", Lang.int minor_heap_size);
        ("major_heap_increment", Lang.int major_heap_increment);
        ("space_overhead", Lang.int space_overhead);
        ("verbose", Lang.int verbose);
        ("max_overhead", Lang.int max_overhead);
        ("stack_limit", Lang.int stack_limit);
        ("allocation_policy", Lang.int allocation_policy);
        ("window_size", Lang.int window_size);
        ("custom_major_ratio", Lang.int custom_major_ratio);
        ("custom_minor_ratio", Lang.int custom_minor_ratio);
        ("custom_minor_max_size", Lang.int custom_minor_max_size);
      ]
  in
  let to_control v =
    let f n = Lang.to_int (Value.invoke v n) in
    {
      Gc.minor_heap_size = f "minor_heap_size";
      major_heap_increment = f "major_heap_increment";
      space_overhead = f "space_overhead";
      verbose = f "verbose";
      max_overhead = f "max_overhead";
      stack_limit = f "stack_limit";
      allocation_policy = f "allocation_policy";
      window_size = f "window_size";
      custom_major_ratio = f "custom_major_ratio";
      custom_minor_ratio = f "custom_minor_ratio";
      custom_minor_max_size = f "custom_minor_max_size";
    }
  in
  ignore
    (Lang.add_builtin ~base:runtime_gc "get" ~category:`System
       ~descr:"Return the current values of the GC parameters" [] control_t
       (fun _ -> control (Gc.get ())));

  Lang.add_builtin ~base:runtime_gc "set" ~category:`System
    ~descr:"Set the GC parameters."
    [("", control_t, None, None)]
    Lang.unit_t
    (fun p ->
      let c = to_control (List.assoc "" p) in
      Gc.set c;
      Lang.unit)

let runtime_sys = Lang.add_module ~base:runtime "sys"

let _ =
  Lang.add_builtin_base ~category:`System
    ~descr:
      "Size of one word on the machine currently executing the program, in \
       bits. Either `32` or `64`."
    ~base:runtime_sys "word_size" (`Int Sys.word_size) Lang.int_t

external file_descriptors_limit : unit -> int
  = "liquidsoap_file_descriptors_limit"
[@@noalloc]

(* /dev/fd lists the calling process' descriptors on Linux and macOS. Reading
   it opens one more descriptor, which is not counted. *)
let open_file_descriptors () =
  try Some (Array.length (Sys.readdir "/dev/fd") - 1) with Sys_error _ -> None

let _ =
  let int_opt = function None -> Lang.null | Some n -> Lang.int n in
  Lang.add_builtin ~base:runtime "file_descriptors" ~category:`System
    ~descr:
      "File descriptors of the process: `count`, the number currently open, \
       and `max`, the maximum number it may open (its soft `RLIMIT_NOFILE` \
       limit). Each is `null` when it cannot be determined on this platform, \
       and `max` is also `null` when there is no limit."
    []
    (Lang.record_t
       [
         ("count", Lang.nullable_t Lang.int_t);
         ("max", Lang.nullable_t Lang.int_t);
       ])
    (fun _ ->
      let max =
        match file_descriptors_limit () with -1 -> None | n -> Some n
      in
      Lang.record
        [("count", int_opt (open_file_descriptors ())); ("max", int_opt max)])

(* GC finalisers can fire at any allocation, in any thread, including one that
   holds a lock script code may take. They only queue the callback, without
   locking; a scheduler task drains the queue. *)
module Finaliser = struct
  let log = Log.make ["runtime"; "gc"]
  let pending : (unit -> unit) list Atomic.t = Atomic.make []
  let drain_interval = 1.

  let rec push fn =
    let current = Atomic.get pending in
    if not (Atomic.compare_and_set pending current (fn :: current)) then push fn

  let log_error ~bt message =
    Utils.log_exception ~log ~bt:(Printexc.raw_backtrace_to_string bt) message

  (* Exiting without starting happens after the log is closed. *)
  let report_error = ref log_error

  let run callback =
    try callback ()
    with exn ->
      let bt = Printexc.get_raw_backtrace () in
      let { Runtime_error.kind; msg; pos } =
        Lang.runtime_error_of_exception ~bt ~kind:"finaliser" exn
      in
      !report_error ~bt
        (Printf.sprintf "Finaliser callback failed%s: %s (%s)"
           (match pos with
             | pos :: _ -> " at " ^ Pos.to_string ~prefix:"" pos
             | [] -> "")
           msg kind)

  let drain () = List.iter run (List.rev (Atomic.exchange pending []))

  let rec task () =
    {
      Scheduler.Task.priority = `Threaded;
      events = [`Delay drain_interval];
      handler =
        (fun _ ->
          drain ();
          if Atomic.get Builtins_thread.should_stop then [] else [task ()]);
    }

  let started = Atomic.make false

  let ensure_started () =
    if not (Atomic.exchange started true) then
      Lifecycle.after_start ~name:"gc finalisers" (fun () ->
          Scheduler.Task.add (task ()))

  (* A callback can drop other watched values: collect again until nothing is
     queued. *)
  let collect_and_drain () =
    let rec loop rounds =
      Gc.full_major ();
      if Atomic.get pending <> [] then (
        drain ();
        if rounds > 1 then loop (rounds - 1))
    in
    if Atomic.get started then loop 8

  let drained_at_shutdown = Atomic.make false

  (* Once the sources are shut down, the values they held are unreachable: one
     last collection runs what they left. A script that never starts, e.g.
     without outputs, exits from the main thread before streaming: exiting then
     drains what is left. After a shutdown, exiting runs nothing, since no
     thread state can be assumed by then. *)
  let () =
    Lifecycle.after_core_shutdown ~name:"gc finalisers" (fun () ->
        Atomic.set drained_at_shutdown true;
        collect_and_drain ());
    at_exit (fun () ->
        if not (Atomic.get drained_at_shutdown) then (
          (report_error := fun ~bt:_ message -> prerr_endline message);
          collect_and_drain ()))

  (* Values that hold no data of their own cannot be watched. *)
  let watch ~pos register v =
    match register () with
      | () -> ensure_started ()
      | exception Invalid_argument _ ->
          log#important
            "Value %s at %s cannot be watched, its finaliser is ignored."
            (Value.to_string v)
            (Pos.List.to_string ~prefix:"" pos)
end

let finalise_descr ~receives =
  Printf.sprintf
    "Call the given function some time after the garbage collector finds the \
     value unreachable, %s, to release what a dropped value held. Every copy \
     of the value, e.g. with methods added, keeps it reachable. Numbers, \
     booleans, null and empty lists cannot be watched: the call is then \
     ignored with a warning. The function runs later on a scheduler thread, \
     may never run, and must not reach the value. A value that stays \
     reachable, such as a top-level definition or a string literal, never runs \
     its function. See the memory usage documentation for details."
    receives

let runtime_gc_finalise =
  let a = Lang.univ_t () in
  Lang.add_builtin ~base:runtime_gc "finalise" ~category:`Liquidsoap
    ~descr:(finalise_descr ~receives:"passing it the value")
    [
      ( "",
        Lang.fun_t [(false, "", a)] Lang.unit_t,
        None,
        Some "Function to call with the value. It must not reach the value." );
      ("", a, None, Some "Value to watch.");
    ]
    Lang.unit_t
    (fun p ->
      let fn = Lang.assoc "" 1 p in
      let v = Lang.assoc "" 2 p in
      Finaliser.watch ~pos:(Lang.pos p)
        (fun () ->
          Value.finalise v (fun v ->
              Finaliser.push (fun () -> ignore (Lang.apply fn [("", v)]))))
        v;
      Lang.unit)

let _ =
  Lang.add_builtin ~base:runtime_gc_finalise "last" ~category:`Liquidsoap
    ~descr:(finalise_descr ~receives:"once it is collected")
    [
      ( "",
        Lang.fun_t [] Lang.unit_t,
        None,
        Some "Function to call. It must not reach the value." );
      ("", Lang.univ_t (), None, Some "Value to watch.");
    ]
    Lang.unit_t
    (fun p ->
      let fn = Lang.assoc "" 1 p in
      let v = Lang.assoc "" 2 p in
      Finaliser.watch ~pos:(Lang.pos p)
        (fun () ->
          Value.finalise_last v (fun () ->
              Finaliser.push (fun () -> ignore (Lang.apply fn []))))
        v;
      Lang.unit)
