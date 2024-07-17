
(** Make plugs self-documenting. *)

module H = Hashtbl

(** A simple representation of documentation *)

class item (size:int) (doc:string) =
object

  val doc = doc
  method get_doc = doc

  val subsections : (string,item) H.t = H.create size
  method get_subsections = H.copy subsections
  method has_subsection name =
    H.mem subsections name
  method add_subsection label item =
    H.add subsections label item
  method list_subsections =
    H.fold (fun k v l -> k::l) subsections []

end

let trivial s = new item 1 s

(** Two functions which print out an [item], used for liquidsoap to generate
  * (part of) its own documentation: *)

let print_xml item =
  let rec print_xml indent doc =
    let prefix = String.make indent ' ' in
      Printf.printf "%s<info>%s</info>\n" prefix doc#get_doc ;
      H.iter (fun k v ->
		Printf.printf "%s<section>\n" prefix ;
		Printf.printf " %s<label>%s</label>\n" prefix k ;
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
      H.iter (fun k v ->
		Printf.printf "%s+ %s\n" prefix k ;
		print (indent+1) v ) doc#get_subsections
  in
    print 0
