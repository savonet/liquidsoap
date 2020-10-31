(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2020 Savonet team

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

open Lang_builtins

let () =
  let s_t =
    let kind = Lang.any in
    Lang.source_t (Lang.kind_type_of_kind_format kind)
  in
  add_builtin "source.init" ~cat:Liq
    ~descr:
      "Simultaneously initialize sources, return the sublist of sources that \
       failed to initialize."
    ~flags:[Lang.Experimental]
    [("", Lang.list_t s_t, None, None)]
    (Lang.list_t s_t)
    (fun p ->
      let l = Lang.to_list (List.assoc "" p) in
      let l = List.map Lang.to_source l in
      let l =
        (* TODO this whole function should be about active sources,
         *   just like source.shutdown() but the language has no runtime
         *   difference between sources and active sources, so we use
         *   this trick to compare active sources and passive ones... *)
        Clock.force_init (fun x -> List.exists (fun y -> Oo.id x = Oo.id y) l)
      in
      Lang.list (List.map (fun x -> Lang.source (x :> Source.source)) l))

let () =
  let log = Log.make ["source"; "dump"] in
  let kind = Lang.univ_t () in
  add_builtin "source.dump" ~cat:Liq
    ~descr:"Immediately encode the whole contents of a source into a file."
    ~flags:[Lang.Experimental]
    [
      ("", Lang.format_t kind, None, Some "Encoding format.");
      ("", Lang.string_t, None, Some "Name of the file.");
      ("", Lang.source_t (Lang.univ_t ()), None, Some "Source to encode");
    ]
    Lang.unit_t
    (fun p ->
      let proto =
        let p = Pipe_output.file_proto (Lang.univ_t ()) in
        List.filter_map (fun (l, _, v, _) -> Option.map (fun v -> (l, v)) v) p
      in
      let proto = ("fallible", Lang.bool true) :: proto in
      let s = Lang.to_source (Lang.assoc "" 3 p) in
      let p = (("id", Lang.string "source_dumper") :: p) @ proto in
      let fo = Pipe_output.new_file_output p in
      fo#get_ready [s];
      fo#output_get_ready;
      log#info "Start dumping source.";
      while s#is_ready do
        fo#output;
        fo#after_output
      done;
      log#info "Source dumped.";
      fo#leave s;
      Lang.unit)
