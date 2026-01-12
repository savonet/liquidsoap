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
