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

open Lang

let mkdoc s k =
  let i = new Doc.item 1 s in
    i#add_subsection "type" (Doc.trivial (Lang.print_kind k)) ;
    i

let _ =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.string_t]
      Lang.string_t
  in
  Lang.builtins#register "^"
    ~doc:(mkdoc "Concatenate strings." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let s1 = Lang.to_string (Lang.assoc "" 1 p) in
                     let s2 = Lang.to_string (Lang.assoc "" 2 p) in
                       String (s1 ^ s2)) }

let _ =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ;
       false,"",Lang.list_t (Lang.product_t
                               Lang.string_t Lang.string_t)]
      Lang.string_t
  in
  Lang.builtins#register "assoc"
    ~doc:(mkdoc "assoc k [...,(k,v),...] = v" kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let k = Lang.to_string (Lang.assoc "" 1 p) in
                     let l = List.map
                               (fun p ->
                                  let (a,b) = Lang.to_product p in
                                    Lang.to_string a, Lang.to_string b)
                               (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.String (try List.assoc k l with _ -> "")) }

let _ =
  let kind =
    Lang.fun_t
      [false,"",Lang.string_t ;
       false,"",Lang.list_t (Lang.product_t
                               Lang.string_t Lang.string_t)]
      Lang.string_t
  in
  Lang.builtins#register "%"
    ~doc:(mkdoc
            ("(pattern % [...,(k,v),...]) replaces in pattern occurences of:\n"^
             " - \'$(k)\' into \"v\";\n"^
             " - \'$(if $(k2),\"a\",\"b\")\' into \"a\" if k2 is found in "^
             "the list, \"b\" otherwise.") kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None;"",None],[],
                   fun p ->
                     let s = Lang.to_string (Lang.assoc "" 1 p) in
                     let l =
                       List.map
                         (fun p ->
                            let a,b = Lang.to_product p in
                              Lang.to_string a, Lang.to_string b)
                         (Lang.to_list (Lang.assoc "" 2 p))
                     in
                       Lang.String
                         (Utils.interpolate (fun k -> List.assoc k l) s)) }

let _ =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.unit_t in
  Lang.builtins#register "system"
    ~doc:(mkdoc "Shell command call." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     ignore (Unix.system (Lang.to_string (List.assoc "" p))) ;
                     Lang.Unit) }

let _ =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
  Lang.builtins#register "get_process_output"
    ~doc:(mkdoc
            "Perform a shell call and return its output."
            kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let chan =
                       Unix.open_process_in
                         (Lang.to_string (List.assoc "" p))
                     in
                     let rec aux s =
                       let more = String.make 128 '?' in
                       let n = input chan more 0 128 in
                         if n = 0 then s else
                           aux (s^(String.sub more 0 n))
                     in
                     let s = aux "" in
                       ignore (Unix.close_process_in chan) ;
                       String s) }

let _ =
  let kind = Lang.fun_t [false,"",Lang.string_t] (Lang.list_t Lang.string_t) in
  Lang.builtins#register "get_process_lines"
    ~doc:(mkdoc
            "Perform a shell call and return the list of its output lines."
            kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let chan =
                       Unix.open_process_in
                         (Lang.to_string (List.assoc "" p))
                     in
                     let rec aux () =
                       match
                         try Some (input_line chan) with End_of_file -> None
                       with
                         | None -> []
                         | Some s -> s::(aux ())
                     in
                     let l = aux () in
                         ignore (Unix.close_process_in chan) ;
                         List (List.map Lang.string l)) }

let _ =
  let kind = Lang.fun_t [true,"indicators",Lang.list_t Lang.string_t;
                         false,"",Lang.string_t] Lang.request_t in
    Lang.builtins#register "request"
      ~doc:(mkdoc "Create a request." kind)
      { pos = dummy_pos ;
        kind = kind ;
        value = FFI (["indicators",Some (Lang.list []); "",None],[],
                     fun p ->
                       let indicators = List.assoc "indicators" p in
                       let initial = Lang.to_string (List.assoc "" p) in
                       let l = String.length initial in
                       let initial =
                         (* Remove trailing newline *)
                         if l > 0 && initial.[l - 1] = '\n' then
                           String.sub initial 0 (l - 1)
                         else
                           initial
                       in
                       let indicators =
                         List.map Lang.to_string (Lang.to_list indicators)
                       in
                       let indicators =
                         List.map Request.indicator indicators
                       in
                         Request (Request.create ~indicators initial)) }

let _ =
  let kind = Lang.fun_t [false,"",Lang.string_t] Lang.string_t in
  Lang.builtins#register "quote"
    ~doc:(mkdoc "Escape shell metacharacters." kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None],[],
                   fun p ->
                     let s = Lang.to_string (List.assoc "" p) in
                       String (Filename.quote s)) }

let _ =
  let protocol_t =
    Lang.fun_t
      [false,"",Lang.string_t ; false,"",Lang.float_t]
      (Lang.list_t Lang.string_t)
  in
  let kind =
    Lang.fun_t [false,"",Lang.string_t ; false,"",protocol_t ] Lang.unit_t
  in
    Lang.builtins#register "add_protocol"
      ~doc:(mkdoc "Register a new protocol." kind)
      { pos = dummy_pos ;
        kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       let name = Lang.to_string (Lang.assoc "" 1 p) in
                       let f = Lang.assoc "" 2 p in
                         Request.protocols#register name
                           { Request.static = false ;
                             Request.resolve =
                               fun arg ~log timeout ->
                                 let l =
                                   Lang.apply f ["",Lang.string arg;
                                                 "",Lang.float timeout]
                                 in
                                   List.map
                                     (fun s ->
                                        Request.indicator (Lang.to_string s))
                                     (Lang.to_list l) } ;
                         Lang.Unit) }

let _ =
  let kind = let t = false,"",Lang.int_t in Lang.fun_t [ t;t;t ] Lang.bool_t in
  Lang.builtins#register "time_in_mod"
    ~doc:(mkdoc ("INTERNAL: time_in_mod(a,b,c) checks that the unix time T "^
                 "satisfies a <= T mod c < b") kind)
    { pos = dummy_pos ;
      kind = kind ;
      value = FFI (["",None;"",None;"",None],[],
                   fun p ->
                     match List.map (fun (_,x) -> Lang.to_int x) p with
                       | [a;b;c] ->
                           let t = Unix.localtime (Unix.time ()) in
                           let t =
                             t.Unix.tm_sec +
                             t.Unix.tm_min * 60 +
                             t.Unix.tm_hour * 60 * 60 +
                             t.Unix.tm_wday * 24 * 60 * 60
                           in
                           let t = t mod c in
                             if a <= b then
                               Bool (a <= t && t < b)
                             else
                               Bool (not (b <= t && t < a))
                       | _ -> assert false) }

let _ =
  let kind =
    Lang.fun_t [ false,"",Lang.bool_t ; false,"",Lang.bool_t] Lang.bool_t
  in
    Lang.builtins#register "and"
      ~doc:(mkdoc "Return the conjunction of its arguments" kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_bool x) p with
                         | [a;b] -> Bool (a && b)
                         | _ -> assert false) } ;
    Lang.builtins#register "or"
      ~doc:(mkdoc "Return the disjunction of its arguments" kind)
      { pos = dummy_pos ; kind = kind ;
        value = FFI (["",None;"",None],[],
                     fun p ->
                       match List.map (fun (_,x) -> Lang.to_bool x) p with
                         | [a;b] -> Bool (a || b)
                         | _ -> assert false) }
