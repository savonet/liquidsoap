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

class item ?(sort=true) (doc:string) =
  let compare (a,_) (b,_) = compare a b in
  let sort =
    if sort then List.stable_sort compare else fun x -> x
  in
object

  val doc = doc
  method get_doc = doc

  val mutable subsections : (string*item) list = []
  method get_subsections = sort subsections
  method get_subsection name = List.assoc name subsections
  method has_subsection name = List.mem_assoc name subsections
  method add_subsection label item = subsections <- subsections@[label,item]
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

let print_xml item =
  let rec print_xml indent doc =
    let prefix =
      Bytes.unsafe_to_string
        (Bytes.make indent ' ')
      in
      Printf.printf "%s<info>%s</info>\n" prefix (xml_escape doc#get_doc) ;
      List.iter
        (fun (k,v) ->
           Printf.printf "%s<section>\n" prefix ;
           Printf.printf " %s<label>%s</label>\n" prefix (xml_escape k) ;
           print_xml (indent+1) v ;
           Printf.printf "%s</section>\n" prefix
        ) doc#get_subsections
  in
    Printf.printf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ;
    Printf.printf "<all>\n" ;
    print_xml 1 item ;
    Printf.printf "</all>\n"

let print_json item =
  let rec json doc =
    let ss = doc#get_subsections in
    if ss = [] then `String doc#get_doc
    else
      let ss = List.map (fun (k,v) -> k, json v) ss in
      let info = doc#get_doc in
      let ss = if info = "(no doc)" then ss else ("info", `String info)::ss in
      `Assoc ss
  in
  Printf.printf "%s\n" (JSON.to_string (json item))

let print : item -> unit =
  let rec print indent doc =
    let prefix =
      Bytes.unsafe_to_string
        (Bytes.make indent ' ')
      in
      Printf.printf "%s%s\n" prefix doc#get_doc ;
      List.iter
        (fun (k,v) ->
           Printf.printf "%s+ %s\n" prefix k ;
           print (indent+1) v
        ) doc#get_subsections
  in
    print 0

let print_lang (i:item) : unit =
  let print_string_split f s =
    String.iter
      (fun c ->
         if c = ' ' then Format.pp_print_space f () else Format.pp_print_char f c)
      s
  in
  Format.printf "@.@[%a@]@." print_string_split i#get_doc ;
  let sub = i#get_subsections in
  let sub =
    Format.printf "@.Type: %s@." (i#get_subsection "_type")#get_doc ;
    List.remove_assoc "_type" sub
  in
  let sub =
    try
      Format.printf "@.Category: %s@." (List.assoc "_category" sub)#get_doc ;
      List.remove_assoc "_category" sub
    with
      | Not_found -> sub
  in
  let rec print_flags sub =
    try
      Format.printf "Flag: %s@." (List.assoc "_flag" sub)#get_doc ;
      print_flags (List.remove_assoc "_flag" sub)
    with
      | Not_found -> sub
  in
  let sub = print_flags sub in
    if sub<>[] then begin
      Format.printf "@.Parameters:@." ;
      List.iter
        (fun (lbl,i) ->
           Format.printf "@. * %s : %s (default: %s)@."
             lbl
             (i#get_subsection "type")#get_doc
             (i#get_subsection "default")#get_doc ;
           if i#get_doc <> "(no doc)" then
             Format.printf "@[<5>     %a@]@." print_string_split i#get_doc)
        sub
    end ;
    Format.printf "@."
