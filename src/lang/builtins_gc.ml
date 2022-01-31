(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2021 Savonet team

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

let () = Lang.add_module "gc"

let () =
  Lang.add_builtin "gc.full_major" ~category:`Liquidsoap
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
        ("forced_major_collections", Lang.int_t);
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
        forced_major_collections;
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
        ("forced_major_collections", Lang.int forced_major_collections);
      ]
  in
  Lang.add_builtin "gc.stat" ~category:`Liquidsoap
    ~descr:
      "Return the current values of the memory management counters. This \
       function examines every heap block to get the statistics." [] stat_t
    (fun _ -> stat (Gc.stat ()));
  Lang.add_builtin "gc.quick_stat" ~category:`Liquidsoap
    ~descr:
      "Same as stat except that `live_words`, `live_blocks`, `free_words`, \
       `free_blocks`, `largest_free`, and `fragments` are set to `0`. This \
       function is much faster than `gc.stat` because it does not need to go \
       through the heap." [] stat_t (fun _ -> stat (Gc.quick_stat ()))

let () =
  Lang.add_builtin "gc.print_stat" ~category:`Liquidsoap
    ~descr:
      "Print the current values of the memory management counters in \
       human-readable form." [] Lang.unit_t (fun _ ->
      Gc.print_stat stdout;
      flush stdout;
      Lang.unit)
