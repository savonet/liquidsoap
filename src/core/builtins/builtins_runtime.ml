(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2022 Savonet team

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

let () =
  Lang.add_builtin "runtime.gc.full_major" ~category:`Liquidsoap
    ~descr:"Trigger full major garbage collection." [] Lang.unit_t (fun _ ->
      Gc.full_major ();
      Lang.unit)

let () =
  let stat_t =
    Lang.record_t
      [
        ("minor_words", Lang.float_t);
        ("promoted_words", Lang.float_t);
        ("major_words", Lang.float_t);
        ("minor_collections", Lang.int_t);
        ("major_collections", Lang.int_t);
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
  Lang.add_builtin "runtime.gc.stat" ~category:`Liquidsoap
    ~descr:
      "Return the current values of the memory management counters. This \
       function examines every heap block to get the statistics." [] stat_t
    (fun _ -> stat (Gc.stat ()));
  Lang.add_builtin "runtime.gc.quick_stat" ~category:`Liquidsoap
    ~descr:
      "Same as stat except that `live_words`, `live_blocks`, `free_words`, \
       `free_blocks`, `largest_free`, and `fragments` are set to `0`. This \
       function is much faster than `gc.stat` because it does not need to go \
       through the heap." [] stat_t (fun _ -> stat (Gc.quick_stat ()))

let () =
  Lang.add_builtin "runtime.gc.print_stat" ~category:`Liquidsoap
    ~descr:
      "Print the current values of the memory management counters in \
       human-readable form." [] Lang.unit_t (fun _ ->
      Gc.print_stat stdout;
      flush stdout;
      Lang.unit)

let () =
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
  Lang.add_builtin "runtime.gc.get" ~category:`Liquidsoap
    ~descr:"Return the current values of the GC parameters" [] control_t
    (fun _ -> control (Gc.get ()));
  Lang.add_builtin "runtime.gc.set" ~category:`Liquidsoap
    ~descr:"Set the GC parameters."
    [("", control_t, None, None)]
    Lang.unit_t
    (fun p ->
      let c = to_control (List.assoc "" p) in
      Gc.set c;
      Lang.unit)

let () =
  Lang.add_builtin_base ~category:`Liquidsoap
    ~descr:
      "Size of one word on the machine currently executing the program, in \
       bits. Either `32` or `64`."
    "runtime.sys.word_size"
    Lang.(Ground (Ground.Int Sys.word_size))
    Lang.int_t
