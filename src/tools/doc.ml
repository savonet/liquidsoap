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

(** Make plugs self-documenting. *)

class item (doc:string) =
object

  val doc = doc
  method get_doc = doc

  val mutable subsections : (string*item) list = []
  method get_subsections = List.sort compare subsections
  method get_subsection name = List.assoc name subsections
  method has_subsection name = List.mem_assoc name subsections
  method add_subsection label item = subsections <- (label,item)::subsections
  method list_subsections = List.map fst subsections

end

let trivial s = new item s

(** Two functions which print out an [item], used for liquidsoap to generate
  * (part of) its own documentation: *)

let xml_escape s =
  let lt = Str.regexp "<" in
  let gt = Str.regexp ">" in
    Str.global_replace lt "&lt;" (Str.global_replace gt "&gt;" s)

let print_xml item =
  let rec print_xml indent doc =
    let prefix = String.make indent ' ' in
      Printf.printf "%s<info>%s</info>\n" prefix (xml_escape doc#get_doc) ;
      List.iter (fun (k,v) ->
		Printf.printf "%s<section>\n" prefix ;
		Printf.printf " %s<label>%s</label>\n" prefix (xml_escape k) ;
		print_xml (indent+1) v ;
		Printf.printf "%s</section>\n" prefix ) doc#get_subsections
  in
    Printf.printf "<all>\n" ;
    print_xml 1 item ;
    Printf.printf "</all>\n"

let print : item -> unit =
  let rec print indent doc =
    let prefix = String.make indent ' ' in
      Printf.printf "%s%s\n" prefix doc#get_doc ;
      List.iter (fun (k,v) ->
		Printf.printf "%s+ %s\n" prefix k ;
		print (indent+1) v ) doc#get_subsections
  in
    print 0

let print_lang (i:item) : unit =
  Printf.printf "%s\n" i#get_doc ;
  let sub = i#get_subsections in
  let sub =
    try
      Printf.printf "Category: %s\n" (List.assoc "category" sub)#get_doc ;
      List.remove_assoc "category" sub
    with
      | Not_found -> sub
  in
  let sub =
    Printf.printf "Type: %s\n" (i#get_subsection "type")#get_doc ;
    List.remove_assoc "type" sub
  in
  let rec print_flags sub =
    try
      Printf.printf "Flag: %s\n" (List.assoc "flag" sub)#get_doc ;
      print_flags (List.remove_assoc "flag" sub)
    with
      | Not_found -> sub
  in
  let sub = print_flags sub in
    if sub<>[] then begin
      Printf.printf "Parameters:\n" ;
      List.iter
        (fun (lbl,i) ->
           Printf.printf "* %s (type %s) (default %s)\n"
             lbl
             (i#get_subsection "type")#get_doc
             (i#get_subsection "default")#get_doc ;
           if i#get_doc <> "(no doc)" then
             Printf.printf "    %s\n" i#get_doc)
        sub
    end
