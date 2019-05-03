(*****************************************************************************

  Liqi, a simple wiki-like langage
  Copyright 2008-2019 Savonet team

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

open Liqi
open Printf

let opening_quotes = ref false

let r_quotes = Str.regexp "\""
let r_subst =
  [
    "&iuml;", "ï";
    "&eacute;", "é";
  ]
let r_subst = List.map (fun (r,s) -> Str.regexp r, s) r_subst

let (!) s =
  let s = List.fold_left (fun s (r,t) -> Str.global_replace r t s) s r_subst in
  let s =
    Str.global_substitute
      (Str.regexp "\"")
      (fun s ->
         opening_quotes := not !opening_quotes;
         if !opening_quotes then "``" else "''")
      s
  in
    s

let rec print_line f l =
  List.iter
    (function
       | Space -> fprintf f " "
       | Word s -> fprintf f "%s" !s
       | Code s ->
           if String.contains s '\n' || String.length s >= 40 then
             fprintf f "```\n%s```\n" s
           else
             fprintf f "`%s`" s
       | HRef (txt,url) -> fprintf f "[%s](%s)" !txt url
       | Em l -> fprintf f "*%a*" print_line l
       | Bf l -> fprintf f "**%a**" print_line l)
    l

let print_doc f =
  let pprinter =
    {
      print_paragraph = (fun f p x -> Printf.fprintf f "%a\n" p x);
      print_list = (fun f p x -> Printf.fprintf f "%a\n" p x);
      print_item = (fun f p x -> Printf.fprintf f "* %a\n" p x);
      print_line = print_line;
    }
  in
    List.iter
      (function
         | Header (n,_,s) ->
             fprintf f "%s %s\n"
               (String.concat "" (Array.to_list (Array.make n "#")))
               !s
         | Paragraph p -> print_paragraph pprinter f p
         | Image (title,url) -> fprintf f "![%s](%s)" title url
         | Antiquote s -> fprintf f "%s" s 
         | Snippet (_,body,language) ->
             let language = match language with
               | Some l -> l
               | None -> ""
             in
             fprintf f "```%s\n%s```\n\n" language body
      )

let print f doc =
  fprintf f "%a\n" print_doc doc
