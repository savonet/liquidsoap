(*****************************************************************************

  Liquidsoap, a programmable audio stream generator.
  Copyright 2003-2019 Savonet team

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

(** Make plugs self-documenting. *)

class item ?(sort = true) (doc : string) =
  let compare (a, _) (b, _) = compare a b in
  let sort = if sort then List.stable_sort compare else fun x -> x in
  object
    val doc = doc

    method get_doc = doc

    val mutable subsections : (string * item) list = []

    method get_subsections = sort subsections

    method get_subsection name = List.assoc name subsections

    method has_subsection name = List.mem_assoc name subsections

    method add_subsection label item =
      subsections <- subsections @ [(label, item)]

    method list_subsections = List.map fst (sort subsections)
  end

let trivial ?sort s = new item ?sort s

let none ?sort () = trivial ?sort "No documentation available."

(** Two functions which print out an [item], used for liquidsoap to generate
  * (part of) its own documentation: *)

let xml_escape s =
  let amp = Str.regexp "&" in
  let lt = Str.regexp "<" in
  let gt = Str.regexp ">" in
  let s = Str.global_replace amp "&amp;" s in
  let s = Str.global_replace gt "&gt;" s in
  let s = Str.global_replace lt "&lt;" s in
  s

let print_xml (doc : item) print_string =
  let rec print_xml indent doc =
    let prefix = Bytes.unsafe_to_string (Bytes.make indent ' ') in
    Printf.kprintf print_string "%s<info>%s</info>\n" prefix
      (xml_escape doc#get_doc) ;
    List.iter
      (fun (k, v) ->
        Printf.kprintf print_string "%s<section>\n" prefix ;
        Printf.kprintf print_string " %s<label>%s</label>\n" prefix
          (xml_escape k) ;
        print_xml (indent + 1) v ;
        Printf.kprintf print_string "%s</section>\n" prefix)
      doc#get_subsections
  in
  print_string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ;
  print_string "<all>\n" ;
  print_xml 1 doc ;
  print_string "</all>\n"

let rec to_json (doc : item) =
  let ss = doc#get_subsections in
  let sanitize s = s in
  if ss = [] then `String (sanitize doc#get_doc)
  else (
    let ss = List.map (fun (k, v) -> (k, to_json v)) ss in
    let info = doc#get_doc in
    let ss =
      if info = "(no doc)" then ss
      else ("_info", `String (sanitize info)) :: ss
    in
    `Assoc ss )

let print_json (doc : item) print_string =
  print_string (JSON.to_string (to_json doc)) ;
  print_string "\n"

let print_functions (doc : item) print_string =
  let doc = to_json doc in
  let to_assoc = function `Assoc l -> l | _ -> assert false in
  let doc = List.assoc "scripting values" (to_assoc doc) in
  let doc = List.tl (to_assoc doc) in
  let functions = ref [] in
  let add (f, _) = functions := f :: !functions in
  List.iter add doc ;
  let functions = List.sort compare !functions in
  List.iter print_string functions

let print_functions_md (doc : item) print_string =
  let doc = to_json doc in
  let to_assoc = function `Assoc l -> l | _ -> assert false in
  let to_string = function `String s -> s | _ -> assert false in
  let doc = List.assoc "scripting values" (to_assoc doc) in
  let doc = List.tl (to_assoc doc) in
  let by_cat = ref [] in
  let add (f, desc) =
    let desc = to_assoc desc in
    let cat =
      try to_string (List.assoc "_category" desc) with Not_found -> ""
    in
    if not (List.mem_assoc cat !by_cat) then by_cat := (cat, ref []) :: !by_cat ;
    let ff = List.assoc cat !by_cat in
    ff := (f, desc) :: !ff
  in
  List.iter add doc ;
  let by_cat = List.sort (fun (c, _) (c', _) -> compare c c') !by_cat in
  let by_cat = List.filter (fun (c, _) -> c <> "") by_cat in
  List.iter
    (fun (cat, ff) ->
      Printf.ksprintf print_string "## %s\n\n" cat ;
      let ff = List.sort (fun (f, _) (f', _) -> compare f f') !ff in
      List.iter
        (fun (f, desc) ->
          let flags = List.filter (fun (n, _) -> n = "_flag") desc in
          let flags = List.map (fun (_, f) -> to_string f) flags in
          if not (List.mem "hidden" flags) then (
            Printf.ksprintf print_string "### `%s`\n\n" f ;
            Printf.ksprintf print_string "%s\n\n"
              (to_string (List.assoc "_info" desc)) ;
            Printf.ksprintf print_string "Type:\n```\n%s\n```\n\n"
              (to_string (List.assoc "_type" desc)) ;
            let args =
              List.filter
                (fun (n, _) ->
                  n <> "_info" && n <> "_category" && n <> "_type"
                  && n <> "_flag")
                desc
            in
            let args =
              List.map
                (fun (n, v) ->
                  let v = to_assoc v in
                  let s =
                    try to_string (List.assoc "_info" v) with Not_found -> ""
                  in
                  let t = to_string (List.assoc "type" v) in
                  let d = to_string (List.assoc "default" v) in
                  (n, s, t, d))
                args
            in
            print_string "Arguments:\n\n" ;
            List.iter
              (fun (n, s, t, d) ->
                let d =
                  if d = "None" then "" else ", which defaults to `" ^ d ^ "`"
                in
                let s = if s = "" then "" else ": " ^ s in
                Printf.ksprintf print_string "- `%s` (of type `%s`%s)%s\n" n t
                  d s)
              args ;
            if List.mem "experimental" flags then
              print_string "\nThis function is experimental.\n" ;
            print_string "\n" ))
        ff)
    by_cat

let print_protocols_md (doc : item) print_string =
  let doc = to_json doc in
  let to_assoc = function `Assoc l -> l | _ -> assert false in
  let to_string = function `String s -> s | _ -> assert false in
  let doc = List.assoc "protocols" (to_assoc doc) in
  let doc = List.tl (to_assoc doc) in
  List.iter
    (fun (p, v) ->
      let v = to_assoc v in
      let info = to_string (List.assoc "_info" v) in
      let syntax = to_string (List.assoc "syntax" v) in
      let static = to_string (List.assoc "static" v) in
      let static =
        if static = "true" then " This protocol is static." else ""
      in
      Printf.ksprintf print_string "### %s\n\n%s\n\nThe syntax is `%s`.%s\n\n"
        p info syntax static)
    doc

let print (doc : item) print_string =
  let rec print indent doc =
    let prefix = Bytes.unsafe_to_string (Bytes.make indent ' ') in
    Printf.ksprintf print_string "%s%s\n" prefix doc#get_doc ;
    List.iter
      (fun (k, v) ->
        Printf.ksprintf print_string "%s+ %s\n" prefix k ;
        print (indent + 1) v)
      doc#get_subsections
  in
  print 0 doc

let print_lang (i : item) =
  let b = Buffer.create 1024 in
  let ff = Format.formatter_of_buffer b in
  let print_string_split f s =
    String.iter
      (fun c ->
        if c = ' ' then Format.pp_print_space f ()
        else Format.pp_print_char f c)
      s
  in
  Format.fprintf ff "@.@[%a@]@." print_string_split
    (Utils.unbreak_md i#get_doc) ;
  let sub = i#get_subsections in
  let sub =
    Format.fprintf ff "@.Type: %s@." (i#get_subsection "_type")#get_doc ;
    List.remove_assoc "_type" sub
  in
  let sub =
    try
      Format.fprintf ff "@.Category: %s@." (List.assoc "_category" sub)#get_doc ;
      List.remove_assoc "_category" sub
    with Not_found -> sub
  in
  let rec print_flags sub =
    try
      Format.fprintf ff "Flag: %s@." (List.assoc "_flag" sub)#get_doc ;
      print_flags (List.remove_assoc "_flag" sub)
    with Not_found -> sub
  in
  let sub = print_flags sub in
  if sub <> [] then (
    Format.fprintf ff "@.Parameters:@." ;
    List.iter
      (fun (lbl, i) ->
        Format.fprintf ff "@. * %s : %s (default: %s)@." lbl
          (i#get_subsection "type")#get_doc
          (i#get_subsection "default")#get_doc ;
        if i#get_doc <> "(no doc)" then
          Format.fprintf ff "@[<5>     %a@]@." print_string_split i#get_doc)
      sub ) ;
  Format.fprintf ff "@." ;
  Format.pp_print_flush ff () ;
  Utils.print_string ~pager:true (Buffer.contents b)
