
type ditem =
  | Header    of (int * string option * string)
  | Paragraph of ((int * (pitem list)) list)
  | Snippet   of (string option * string)
  | Image     of (string * string)

and pitem =
  | Space
  | Word of string
  | Code of string
  | HRef of (string * string)
  | Em   of pitem list
  | Bf   of pitem list

type doc = ditem list

open Printf

let rec sub_p acc test = function
  | [] -> List.rev acc, []
  | (i,l)::tl when test i -> sub_p ((i,l)::acc) test tl
  | par -> List.rev acc, par

let sub_p = sub_p []

type 'a paragraph_printer = {
  print_paragraph : out_channel -> (out_channel -> 'a -> unit) -> 'a -> unit;
  print_list : out_channel -> (out_channel -> 'a -> unit) -> 'a -> unit;
  print_item : out_channel -> (out_channel -> 'a -> unit) -> 'a -> unit;
  print_line : out_channel -> pitem list -> unit;
}

let rec print_paragraph pprinter ?(cur=0) f = function
  | [] -> ()
  | (i,l)::tl as p ->
       if i=0 then
         let p1,p2 = sub_p (fun j -> j=0) p in
           pprinter.print_paragraph f
             (fun f ->
                List.iter
                  (fun (_,l) ->
                     pprinter.print_line f l ;
                     fprintf f "\n"))
             p1 ;
           print_paragraph ~cur pprinter f p2
       else
         if i=cur then
           let p1,p2 = sub_p (fun j -> j>i) tl in
             pprinter.print_item f
               (fun f p1 ->
                  pprinter.print_line f l ;
                  print_paragraph ~cur pprinter f p1)
               p1;
             print_paragraph ~cur pprinter f p2
         else
           let p1,p2 = sub_p (fun j -> j>=i) p in
             assert (i>cur) ;
             pprinter.print_list f (print_paragraph pprinter ~cur:i) p1 ;
             print_paragraph pprinter ~cur f p2
