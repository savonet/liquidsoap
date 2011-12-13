(*****************************************************************************

  Liqi, a simple wiki-like langage
  Copyright 2008-2011 Savonet team

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

let print_template f template_file subst print_body =
  let c = open_in template_file in
  let rec loop () =
    let l = input_line c ^ "\n" in
      begin try
        let s = Pcre.exec ~pat:"(.*)@body@(.*)" l in
          output_string f (Pcre.get_substring s 1) ;
          print_body f ;
          output_string f (Pcre.get_substring s 2)
      with
        | Not_found ->
            output_string f
              (Pcre.substitute_substrings
                 ~pat:"@([a-z]+)@"
                 ~subst:(fun s ->
                           try List.assoc (Pcre.get_substring s 1) subst
                           with Not_found -> "")
                 l)
      end ;
      loop ()
  in
    try loop () with End_of_file -> close_in c

module S = Set.Make (String)

let add_snippet,regenerate_snippet_index =
  let i = ref None in
  let get () =
    let f = open_in "scripts/index.txt" in
    let rec loop s =
      match try Some (input_line f) with _ -> None with
        | Some l ->
            begin match
              if l<>"" && l.[0]='*' then
                let i = String.index_from l 0 '"' in
                let j = String.index_from l (i+1) '"' in
                let _ = String.index_from l j ':' in
                  Some (String.sub l (i+1) (j-i-1))
              else
                None
            with
              | Some l -> loop (S.add l s)
              | None -> loop s
            end
        | None -> i := Some s
    in
      loop S.empty ;
      close_in f
  in
  let get () =
    match !i with
      | None ->
          (try get () with Sys_error _ -> i := Some S.empty) ;
          begin match !i with
            | Some l -> l
            | None -> assert false
          end
      | Some l -> l
  in
  let set () =
    if !i<>None then
      let f = open_out "scripts/index.txt" in
        S.iter
          (fun l ->
             let html = Filename.chop_extension l ^ ".html" in
               fprintf f "* %S:%s\n" l html)
          (get ()) ;
        close_out f
  in
    (fun filename -> i := Some (S.add filename (get ()))),
    set

(** {Rewriting} *)

let r_quotes = Str.regexp "\""

(* Substitutions to be applied always, even in code. *)
let r_subst_pre =
  List.map (fun (r,s) -> Str.regexp r, s)
    [
      "<", "&lt;";
      ">", "&gt;"
    ]

(** Substitutions to be applied only on text. *)
let r_subst =
  List.map (fun (r,s) -> Str.regexp r, s)
    [
      "&lambda;", "λ";
      "&", "&amp;";
      "--", "&ndash;"
    ]

let opening_quotes = ref false
let rewrite pre s =
  let s =
    if pre then s else
      Str.global_substitute
        r_quotes
        (fun s ->
           opening_quotes := not !opening_quotes;
           if !opening_quotes then "“" else "”")
        (List.fold_left (fun s (r,t) -> Str.global_replace r t s) s r_subst)
  in
    List.fold_left (fun s (r,t) -> Str.global_replace r t s) s r_subst_pre

let (!) = rewrite false
let (!!) = rewrite true

let r_http_url = Str.regexp "^http://"

let rec print_line f l =
  List.iter
    (function
       | Space -> fprintf f " "
       | Word s -> fprintf f "%s" !s
       | Code s -> fprintf f "<code>%s</code>" !!s
       | HRef (txt,url) ->
           begin
             (* This is a quick help for admins who want to check links. *)
             let f = open_out_gen [Open_append;Open_creat] 0o644 "links.txt" in
               fprintf f "%s\n" url ; close_out f
           end ;
         fprintf f "<a href=\"%s\"%s>%s</a>"
           url
           (if Str.string_match r_http_url url 0 then
              " target=\"_blank\""
            else
              "")
           !txt
       | Em l -> fprintf f "<em>%a</em>" print_line l
       | Bf l -> fprintf f "<b>%a</b>" print_line l)
    l

let print_snippet ~snippet_template ~subst f title snippet =
  print_template f snippet_template
    (subst @
     ["title",!title])
    (fun f -> output_string f snippet)

let print_doc ?snippet_template ~subst ~basedir f =
  let pprinter =
    {
      print_paragraph = (fun f p x -> Printf.fprintf f "<p>\n%a</p>\n" p x);
      print_list = (fun f p x -> Printf.fprintf f "<ul>\n%a</ul>\n" p x);
      print_item = (fun f p x -> Printf.fprintf f "<li>\n%a</li>\n" p x);
      print_line = print_line;
    }
  in
  let extract_suffix s =
    try
      (Pcre.extract ~rex:(Pcre.regexp "\\.(.+)$") s).(1)
    with _ -> ""
  in
    fun doc -> List.iter
      (function
         | Header (n,None,s) -> fprintf f "<h%d>%s</h%d>\n" n !s n
         | Header (n,Some a,s) ->
             fprintf f "<h%d><a name=%S>%s</a></h%d>\n" n a !s n
         | Paragraph p -> print_paragraph pprinter f p
         | Image (title,url) ->
             fprintf f "<img alt=%S src=\"%s%s\" />" title basedir url
         | Antiquote aq -> fprintf f "%s" aq
         | Snippet (Some title,body, language) ->
             add_snippet title ;
             begin
               let script = open_out ("scripts/"^title) in
                 output_string script body ;
                 close_out script ;
             end ;
             begin match snippet_template with
               | Some snippet_template ->
                   let html = Filename.chop_extension title ^ ".html" in
                   let script = open_out ("scripts/"^html) in
                     print_snippet ~snippet_template ~subst script title body ;
                     close_out script
               | None -> ()
             end ;
             let klass = 
               match language with
                 | None -> extract_suffix title
                 | Some x -> x
             in
             fprintf f "<pre class=\"syntax %s\">%s</pre>\n" klass !!body;
             fprintf f "<div align=\"right\">\n\
                        <a href=\"scripts/%s\">\n\
                          <img class=\"grab\" src=\"%simages/grab.png\" \
                               alt=\"Grab the code!\">\n\
                        </a>\n\
                        </div></p>\n" title basedir
         | Snippet (None, body, language) ->
             let klass =
               match language with
                 | None -> ""
                 | Some x -> x
             in
             fprintf f "<pre class=\"syntax %s\">%s</pre>\n" klass !!body)
      doc ;
    regenerate_snippet_index ()

let print ~template ~subst ?snippet_template ~basedir f ?title doc =
  print_template f template
    (subst @ ["title",(match title with None -> "" | Some t -> !t)])
    (fun f -> print_doc ?snippet_template ~subst ~basedir f doc)
